/* Copy buttons for the protein-name dropdowns. Loaded through the
 * htmlDependency attached by copyable_select() in R/utils-copyable-select.R.
 * The dropdowns are rebuilt by renderUI(), so every handler is attached to
 * `document` instead of to individual elements.
 */
(function () {
  var MESSAGE_DURATION_MS = 1400;

  function findSelectForButton(button) {
    var wrapper = button.closest(".copyable-select");
    return wrapper ? wrapper.querySelector("select") : null;
  }

  // Labels, not values: the QC selector's "ALL ANALYTES" has value "allonly".
  function getSelectedLabels(select) {
    if (!select) return "";
    if (select.selectize) {
      return select.selectize.$control
        .find(".item")
        .map(function () {
          return this.textContent.trim();
        })
        .get()
        .filter(Boolean)
        .join("; ");
    }
    return Array.prototype.slice
      .call(select.selectedOptions || [])
      .map(function (option) {
        return option.text.trim();
      })
      .filter(Boolean)
      .join("; ");
  }

  // navigator.clipboard only works over https or localhost.
  function copyToClipboard(text) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text);
    }
    return new Promise(function (resolve, reject) {
      var previouslyFocused = document.activeElement;
      var hiddenTextarea = document.createElement("textarea");
      hiddenTextarea.value = text;
      hiddenTextarea.setAttribute("readonly", "");
      hiddenTextarea.style.position = "fixed";
      hiddenTextarea.style.top = "-1000px";
      hiddenTextarea.style.left = "-1000px";
      hiddenTextarea.style.opacity = "0";
      document.body.appendChild(hiddenTextarea);
      hiddenTextarea.select();
      var copied = false;
      try {
        copied = document.execCommand("copy");
      } catch (e) {
        copied = false;
      }
      document.body.removeChild(hiddenTextarea);
      if (previouslyFocused && previouslyFocused.focus) previouslyFocused.focus();
      return copied ? resolve() : reject(new Error("execCommand copy failed"));
    });
  }

  function showTemporaryMessage(button, message) {
    var tooltip = button.querySelector(".copyable-select-tip");
    if (!tooltip) return;
    if (button.dataset.originalTooltip === undefined) {
      button.dataset.originalTooltip = tooltip.textContent;
    }
    tooltip.textContent = message;
    button.classList.add("is-showing-message");
    clearTimeout(button.messageTimer);
    button.messageTimer = setTimeout(function () {
      button.classList.remove("is-showing-message");
      tooltip.textContent = button.dataset.originalTooltip;
    }, MESSAGE_DURATION_MS);
  }

  function updateButtonEnabledState(wrapper) {
    var button = wrapper.querySelector(".copyable-select-btn");
    if (!button) return;
    button.disabled = getSelectedLabels(wrapper.querySelector("select")) === "";
  }

  function updateAllButtons() {
    var wrappers = document.querySelectorAll(".copyable-select");
    Array.prototype.forEach.call(wrappers, updateButtonEnabledState);
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest
      ? event.target.closest(".copyable-select-btn")
      : null;
    if (!button) return;
    var text = getSelectedLabels(findSelectForButton(button));
    if (!text) {
      showTemporaryMessage(button, "Nothing selected");
      return;
    }
    copyToClipboard(text).then(
      function () {
        showTemporaryMessage(button, "Copied");
      },
      function () {
        showTemporaryMessage(button, "Copy failed");
      }
    );
  });

  // Plain selects fire a native change event.
  document.addEventListener("change", function (event) {
    var wrapper =
      event.target && event.target.closest
        ? event.target.closest(".copyable-select")
        : null;
    if (wrapper) updateButtonEnabledState(wrapper);
  });

  // Selectize does not, so this also covers new and updated selectize inputs.
  if (window.jQuery) {
    jQuery(document).on("shiny:bound shiny:value shiny:inputchanged", function () {
      setTimeout(updateAllButtons, 0);
    });
  }
  document.addEventListener("DOMContentLoaded", updateAllButtons);
})();
