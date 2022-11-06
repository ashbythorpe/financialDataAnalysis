$(function() {
  $(".v_numeric_input").on("shiny:inputchanged", _.debounce(function(event) {
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
  $("#row_container").on("shiny:inputchanged", ".custom_update", function(event) {
    const id = $(this).attr('data-id');
    // Notify the server that a row has been changed ()
    Shiny.setInputValue(id, 0, {priority: 'event'});
  });
  
  $("#row_container").on("click", ".add_row", function(event) {
    const id = $(this).attr("data-id");
    const n = $(this).attr("data-n");
    // Tell the server the row number of the add_row event (n)
    Shiny.setInputValue(id, n, {priority: 'event'});
  });
  
  $("#row_container").on("click", ".delete_row", function(event) {
    const id = $(this).attr("data-id");
    const n = $(this).attr("data-n");
    Shiny.setInputValue(id, n, {priority: 'event'});
  });
});
