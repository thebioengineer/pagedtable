HTMLWidgets.widget({

  name: "pagedtable",

  type: "output",

  factory: function(el, width, height) {
    var pagedtable_contents;
    var pagedtable;


    // create our sigma object and bind it to the element
    return {
      renderValue: function(x) {
        pagedtable_contents = x;
        // create table
        pagedtable = new PagedTable(el.id, pagedtable_contents);
        pagedtable.init();

      },

      resize: function(width, height) {
        pagedtable.updateView();
      },

      pt: pagedtable
    };
  }
});
