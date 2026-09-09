/* Copy-to-clipboard support for the protein-name dropdowns.
 *
 * Every call site renders inside renderUI(), so the select elements are
 * created and destroyed as the user switches plot types. All handlers are
 * therefore delegated from `document` - nothing is bound per element.
 *
 * The button reaches its select by DOM traversal rather than by id, so the
 * R helper stays a pure wrapper and does not need the namespaced inputId.
 */
(function () {
  var FLASH_MS = 1400;

  function selectFor(btn) {
    var wrap = btn.closest(".copyable-select");
    return wrap ? wrap.querySelector("select") : null;
  }

  /* Copy what the user SEES, not the underlying value. The QC plot selector
     offers choices = c("ALL ANALYTES" = "allonly", ...), where the value is
     "allonly" - copying that would be meaningless. For real protein names
     label and value are identical, so reading the label is correct in both
     cases. multiple = TRUE selects are joined. */
  function selectedText(sel) {
    if (!sel) return "";
    if (sel.selectize) {
      return sel.selectize.$control
        .find(".item")
        .map(function () {
          return this.textContent.trim();
        })
        .get()
        .filter(Boolean)
        .join("; ");
    }
    return Array.prototype.slice
      .call(sel.selectedOptions || [])
      .map(function (o) {
        return o.text.trim();
      })
      .filter(Boolean)
      .join("; ");
  }

  /* navigator.clipboard needs a secure context. 127.0.0.1 qualifies, but an
     instance served over plain http on a lab host does not, hence the
     execCommand fallback. */
  function copyText(text) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text);
    }
    return new Promise(function (resolve, reject) {
      var restoreFocus = document.activeElement;
      var ta = document.createElement("textarea");
      ta.value = text;
      ta.setAttribute("readonly", "");
      ta.style.position = "fixed";
      ta.style.top = "-1000px";
      ta.style.left = "-1000px";
      ta.style.opacity = "0";
      document.body.appendChild(ta);
      ta.select();
      var ok = false;
      try {
        ok = document.execCommand("copy");
      } catch (e) {
        ok = false;
      }
      document.body.removeChild(ta);
      /* ta.select() stole focus; put it back so keyboard users keep their place. */
      if (restoreFocus && restoreFocus.focus) restoreFocus.focus();
      return ok ? resolve() : reject(new Error("execCommand copy failed"));
    });
  }

  function flash(btn, msg) {
    var tip = btn.querySelector(".copyable-select-tip");
    if (!tip) return;
    if (btn.dataset.restoreTip === undefined) {
      btn.dataset.restoreTip = tip.textContent;
    }
    tip.textContent = msg;
    btn.classList.add("is-flashing");
    clearTimeout(btn.flashTimer);
    btn.flashTimer = setTimeout(function () {
      btn.classList.remove("is-flashing");
      tip.textContent = btn.dataset.restoreTip;
    }, FLASH_MS);
  }

  /* Keyed on the wrapper, not on a select: iterating selects would let a
     wrapper holding more than one select disagree with selectFor() about which
     select the button represents. copyable_select() rejects that case on the R
     side; this keeps the two ends consistent regardless. */
  function refresh(wrap) {
    var btn = wrap.querySelector(".copyable-select-btn");
    if (!btn) return;
    btn.disabled = selectedText(wrap.querySelector("select")) === "";
  }

  function refreshAll() {
    var wraps = document.querySelectorAll(".copyable-select");
    Array.prototype.forEach.call(wraps, refresh);
  }

  document.addEventListener("click", function (e) {
    var btn = e.target.closest ? e.target.closest(".copyable-select-btn") : null;
    if (!btn) return;
    /* No preventDefault/stopPropagation here: a click on the button never
       reaches the sibling select anyway, and stopPropagation would suppress
       any window-level click handler the host page installs. */
    var text = selectedText(selectFor(btn));
    if (!text) {
      flash(btn, "Nothing selected");
      return;
    }
    copyText(text).then(
      function () {
        flash(btn, "Copied");
      },
      function () {
        flash(btn, "Copy failed");
      }
    );
  });

  /* Fallback for plain (selectize = FALSE) selects, which do emit a native
     change event. Selectize inputs never reach this: selectize signals via
     jQuery .trigger("change"), which dispatches no native event for <select>.
     Those are handled by the shiny:* hooks below, which are the primary path. */
  document.addEventListener("change", function (e) {
    var wrap =
      e.target && e.target.closest ? e.target.closest(".copyable-select") : null;
    if (wrap) refresh(wrap);
  });

  /* renderUI insertion and value updates both land here. */
  if (window.jQuery) {
    jQuery(document).on("shiny:bound shiny:value shiny:inputchanged", function () {
      setTimeout(refreshAll, 0);
    });
  }
  document.addEventListener("DOMContentLoaded", refreshAll);
})();
