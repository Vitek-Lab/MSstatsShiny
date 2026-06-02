# MSstatsShiny scaling: cost breakdown + architecture options

## 1. Current AWS spend

Cost Explorer, 3-month average (Feb-Apr 2026), `olga-vitek-lab` account, `us-east-1`:

| Service | Monthly avg | Notes |
|---|---|---|
| EC2-Instances | $27 | One `t3a.medium` running 24/7 (`i-0908994d2d2186f7a`, "nu-ov-lab-us-MSstatsShiny") |
| EC2-Other | $22 | EBS volumes + Elastic IP |
| VPC | $10 | Public IPv4 charges and/or VPC endpoints |
| S3 | $8-15 | Lab buckets; April spiked to $14.87 |
| Route 53 | $0.50 | Existing hosted zone for `msstatsshiny.com` |
| All others | $0 | CloudWatch, KMS, SNS, SQS - unused |
| **Total** | **~$70/month** | |

One thing from the audit worth flagging: the existing CloudWatch `StatusCheckFailed` alarm targets a stale instance ID (`i-068bfdd11cb2703f2`), not the live production instance. The lab has no working monitoring on the site today. The new design fixes this as a side effect.

---

## 2. Projected cost for the new architecture

The new setup adds an ALB, Fargate, WAF, CloudWatch logs, and ECR. Both architecture options share the same fixed costs.

### 2.1 Shared fixed costs

| Component | Monthly | Notes |
|---|---|---|
| Application Load Balancer | ~$22 | $16.43 hourly + ~5 LCU |
| Route 53 | $0.50 | Existing zone |
| ACM certificate | $0 | Free, auto-renewing |
| ECR storage | $0.50 | ~5 GB of image versions |
| CloudWatch Logs | ~$5 | ~5 GB/month ingested |
| S3 (ALB access logs) | ~$1 | |
| AWS WAF | ~$8 | ACL + 3 managed rules + ~1M requests |
| **Subtotal** | **~$37/month** | |

### 2.2 Variable cost: Fargate compute

Sizing assumptions:
- **Option A**: ShinyProxy dispatcher always-on at 0.5 vCPU / 1 GB; user tasks spawned on demand at Fargate's floor of 0.25 vCPU / 0.5 GB.
- **Option B**: 2 tasks always-on at 1 vCPU / 2 GB for HA; autoscales up to 8 tasks under load.

Usage pattern: site mostly idle, with one ~5-day short course per month (~50 concurrent users for ~8 hours/day).

| Scenario | Option A | Option B |
|---|---|---|
| Idle month | ~$55/mo | ~$109/mo |
| Light steady traffic (~5 users) | ~$60/mo | ~$109/mo |
| Course month (one 5-day course) | ~$80/mo | ~$115/mo |
| Course month at hard cap (75 concurrent) | ~$95/mo | ~$125/mo |

Option A is $30-50/month cheaper because user containers only run when users are active; Option B carries a ~$72/mo always-on baseline for its 2 pre-warmed tasks.

**Caveat**: these numbers assume the sizing above. CloudWatch shows the current EC2 at 0.3% baseline CPU - dramatically over-provisioned. Local profiling (in progress) will likely let us shrink task sizes and cut costs further.

**On the $150-200 framing**: when I floated $150-200 as a budget envelope earlier, that was an engineering-side guess - not a target the architecture forces. Realistic operating range is $80-125/month, so you can set a tighter ceiling if that fits the lab's grants better.

---

## 3. Cost vs concurrent users

Marginal cost of adding concurrent users (sustained for an 8-hour course session), on top of the baseline:

| Concurrent users | Option A | Option B |
|---|---|---|
| 1 | +$2 | $0 (within baseline) |
| 5 | +$5 | $0 (within baseline) |
| 10 | +$10 | +$1 (one extra task) |
| 25 | +$25 | +$3 (one to two extra tasks) |
| 50 | +$50 | +$6 (three extra tasks) |
| 75 (hard cap) | +$74 | +$12 (six extra tasks at cap) |

Option A scales linearly (each user = one task). Option B scales in steps (one task serves ~10 users, TBD with profiling). At 50 users, Option B is much cheaper per user; below ~10 users, Option A wins because nothing runs when nobody's active.

---

## 4. ShinyProxy on Fargate: Option A vs Option B

The key choice: do we keep ShinyProxy as a per-user dispatcher (current model, ported to Fargate), or replace it with multiple parallel Shiny tasks behind ALB sticky sessions?

### 4.1 Option A - ShinyProxy on Fargate (one container per user)

ShinyProxy runs as a long-lived Fargate task. When a user connects, it spawns a new Fargate task running the Shiny container, routes the user to it, and tears the task down when they disconnect.

**Pros**
1. **Per-user isolation.** Each user gets their own container - one user's heavy computation, crash, or memory spike doesn't affect others. Per-user resource caps are possible.
2. **Lower idle cost.** Only the lightweight dispatcher runs 24/7; user containers consume nothing when nobody's active.
3. **Matches the current operational model.** Minimal mental shift for the team; the `application.yml` mental model carries over.

**Cons**
1. **Cold-start latency.** Fargate task spin-up is 30-60 seconds. Every new user waits on a loading screen on first hit - bad UX for a demo site where first-time visitors are the whole point.
2. **~3× the CDK complexity.** Needs a ShinyProxy task definition, IAM role for dynamic ECS task launches, networking for spawned tasks, plus ongoing `application.yml` management.
3. **Open-source dispatcher, no AWS support.** Integration issues between ShinyProxy and the Fargate ECS backend are on us to debug.
4. **JVM and ECR overhead.** ShinyProxy adds 500 MB - 1 GB memory baseline regardless of users; every spawned session pulls the image.

### 4.2 Option B - Multi-task Shiny service with ALB sticky sessions

Multiple Shiny Fargate tasks run in parallel, pre-warmed. The ALB uses cookie-based stickiness to bind each user to one task for their session. ECS autoscales task count on CPU and request volume.

**Pros**
1. **No cold-start.** Tasks are pre-warmed; users get instant page loads.
2. **Simpler overall.** Standard ECS service pattern, ~1/3 the CDK code of Option A, standard CloudWatch metrics, no dispatcher single-point-of-failure.
3. **Native autoscaling.** ECS scales on CPU / request count; no custom dispatcher logic.

**Cons**
1. **Shared R process per task.** Multiple users share one task's resources, with no per-user caps. A heavy computation slows others on the same task; a crash impacts all ~10 users on that task.
2. **Sticky session fragility.** Users who clear cookies or switch networks mid-session may be re-routed and lose state.
3. **Higher idle cost.** 2 tasks run 24/7 for HA even with zero users.
4. **Capacity planning needs profiling.** "Users per task" is determined empirically.

---

## 5. Two decisions requested

**Decision 1 - Architecture: Option A or Option B?**

Both fit comfortably under recommended budget. The trade-off is cold-start UX (Option A's biggest con) vs per-user isolation and lower cost (Option A's pros).

**Decision 2 - Cost ceiling.** Three reasonable framings, all with hard caps and AWS Budgets alerts to prevent runaway spend:
- (a) **~$125/month** - tight ceiling, optimizes for cost
- (b) **~$150/month** - comfortable for either option
- (c) **~$200/month** - generous headroom for future growth