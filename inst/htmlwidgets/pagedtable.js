HTMLWidgets.widget({

  name: "pagedtable",

  type: "output",

  factory: function(el, width, height) {

    // create our sigma object and bind it to the element
    return {

      renderValue: function(x) {
        // create table
        // create table
        var  pagedTable = new PagedTable(el.id, x);
        pagedTable.init();

      }
    };
  }
});
