$(function() {
  $(".v_numeric_input").on("shiny:inputchanged", debounce(function(event) {
    const min = $(this).attr('min');
    const max = $(this).attr('max');
    const val = event.value
    if(val < min) {
      $(this).val(min).trigger('change');
    } else if(val > max) {
      $(this).val(max).trigger('change');
    };
  }, 1000));
  
  // Delegated event handler can detect rows that are added after this handler 
  // is created (while a direct handler would not).
  $("#custom_row_container").on("click", ".add_row", function(event) {
    const id = $(this).attr("data-id");
    const n = $(this).attr("data-n");
    // Tell the server the row number of the add_row event (n)
    Shiny.setInputValue(id, n, {priority: 'event'});
  });
  
  $("#custom_row_container").on("click", ".delete_row", function(event) {
    const id = $(this).attr("data-id");
    const n = $(this).attr("data-n");
    Shiny.setInputValue(id, n, {priority: 'event'});
  });
  
  // Don't need an add_filter event handler because filters are created using
  // a different method.
  $("#filter_container").on("click", ".delete_filter", function(event) {
    const id = $(this).attr("data-id");
    const n = $(this).attr("data-n");
    Shiny.setInputValue(id, n, {priority: 'event'});
  });
  
  // Callback when the minimum value of a filter is changed
  $("#filter_container").on("shiny:inputchanged", ".filter_min", function(event) {
    var min = event.value;
    const n = $(this).attr("data-n");
    const slider_selector = ".filter_range[data-n = " + n + "]";
    const max_selector = ".filter_max[data-n = " + n + "]";
    const from = $(slider_selector).data("ionRangeSlider").from;
    const max = $(max_selector).val();
    
    if(min > max){
      min = max;
    };
    
    const min_valid = min >= $(this).attr("min") && min <= $(this).attr("max");
    if(min != from && min_valid) {
      // Update the slider to match the value
      $(slider_selector).data("ionRangeSlider").update({from:min});
      // Update the minimum value of the maximum, so that min <- max
      $(max_selector).attr("min", min);
    };
  });
  
  $("#filter_container").on("shiny:inputchanged", ".filter_max", function(event) {
    var max = event.value;
    const n = $(this).attr("data-n");
    const slider_selector = ".filter_range[data-n = " + n + "]";
    const min_selector = ".filter_min[data-n = " + n + "]";
    const to = $(slider_selector).data("ionRangeSlider").to;
    const min = $(min_selector).val();
    
    if(max < min){
      max = min;
    };
    
    const max_valid = max >= $(this).attr("min") && max <= $(this).attr("max");
    if(max != to && max_valid) {
      // Update the slider to match the value
      $(slider_selector).data("ionRangeSlider").update({to:max});
      // Update the minimum value of the maximum, so that min <- max
      $(min_selector).attr("max", max);
    };
  });
  
  // Change the onFinish callback for any filter sliders that are created
  $("#filter_container").on("shiny:bound", ".filter_range", function(event) {
    $(this).data("ionRangeSlider").update({onFinish:slider_on_finish});
  });
  
  create_tooltip(
    "combine",
    "Turn this on to combine your uploaded data with a large table of stock data"
  );
  
  create_tooltip(
    "x_coord",
    "Corresponds to a specific value in the column"
  );
  
  create_tooltip(
    "y_coord",
    "The score (between 0 and 1) that the column value will result in"
  );
  
  create_tooltip(
    "score_type",
    "The method to use when creating a score"
  );
  
  create_tooltip(
    "colname",
    "The column in your data to score"
  );
  
  create_tooltip(
    "score_name",
    "The name of the resulting score"
  );
  
  create_tooltip(
    "weight",
    "The weight of the score when calculating the final score (a weighted mean of the other scores)"
  );
  
  create_tooltip(
    "linear_lb",
    "If the value of the column is less than or equal to this, the resulting score is 0"
  );
  
  create_tooltip(
    "linear_ub",
    "If the value of the column is more than or equal to this, the resulting score is 1"
  );
  
  create_tooltip(
    "peak_lb",
    "If the value of the column is less than or equal to this, the resulting score is 0"
  );
  
  create_tooltip(
    "peak_ub",
    "If the value of the column is more than or equal to this, the resulting score is 0"
  );
  
  create_tooltip(
    "centre",
    "The closer the value of the column is to this, the closer the score is to 1"
  );
  
  create_tooltip(
    "inverse",
    "Invert the score. The lower bound and upper bound produce a score of 1, and the centre produces a score of 0."
  );
  
  create_tooltip(
    "exponential",
    "Whether to apply an exponential transformation to the score"
  );
  
  create_tooltip(
    "logarithmic",
    "Whether to invert the transformation"
  );
  
  create_tooltip(
    "magnitude",
    "The magnitude of the transformation: A higher number means that the transformation will have a bigger effect"
  );
  
  create_tooltip(
    "frequency",
    "How often predictions should be made"
  );
  
  create_tooltip(
    "interactive",
    "Will improve performance and may produce better custom plots"
  );
  
  create_tooltip(
    "show_text",
    "Useful for data with a small number of columns, but can cause clutter otherwise"
  )
});

Shiny.addCustomMessageHandler("files_reset", function(id) {
  const selector = '#' + id;
  const selector2 = selector + "_progress";
  $(selector).val('');
  $(selector2).css("visibility", "hidden");
  $(selector2).find(".progress-bar").css("width", "0");
  $(selector).closest(".input-group").find("input[type='text']").val('');
});

Shiny.addCustomMessageHandler("filters_update", function(id) {
  Shiny.setInputValue(id, 0, {priority: 'event'});
});

Shiny.addCustomMessageHandler("custom_update", function(id) {
  Shiny.setInputValue(id, 0, {priority: 'event'});
});

// Check if two numbers are close enough for one to be a rounded version of the 
// other
function close_enough(x, y, step) {
  return Math.abs(x - y) < step;
};

// Run when the slider of a filter is changed
function slider_on_finish(data) {
  const from = data.from;
  const to = data.to;
  const element = data.input;
  const n = element.attr("data-n");
  const step = element.attr("data-step");
  const min_selector = ".filter_min[data-n = " + n + "]";
  const max_selector = ".filter_max[data-n = " + n + "]";
  const min = $(min_selector).val();
  const max = $(max_selector).val();
  
  if(!close_enough(min, from, step)) {
    // Update the minimum value to match the slider
    $(min_selector).val(from).trigger('change');
    $(max_selector).attr('min', from);
  };
  
  if(!close_enough(max, to, step)) {
    // Update the maximum value to match the slider
    $(max_selector).val(to).trigger('change');
    $(min_selector).attr('max', to);
  };
};

// https://stackoverflow.com/a/57763036
function debounce(callback, wait) {
  let timeout;
  return (...args) => {
      clearTimeout(timeout);
      timeout = setTimeout(function () { callback.apply(this, args); }, wait);
  };
};

function create_tooltip(id, content) {
  ns_id = "tooltip-" + id;
  tippy("#" + ns_id, {
    content: content,
    arrow: false,
    theme: "FDA"
  });
};
