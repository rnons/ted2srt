(function() {
  'use strict';
  var app = window.app || (window.app = {});
  app.utils = {};

  app.utils.parseQueryString = function() {
    if (!document.location.search) return;
    var querys = document.location.search.split('?')[1].split('&');
    var params = {};
    querys.forEach(function(q) {
      var qs, key, value
      qs = q.split('=');
      key = qs[0];
      value = qs[1];
      if (params[key]) {
        if (!(params[key] instanceof Array)) {
          params[key] = [params[key]];
        }
        if (params[key].indexOf(value) === -1) {
          params[key].push(value);
        }
      } else {
        params[key] = value;
      }
    });
    return params;
  };

  app.utils.pprDate = function(dateString) {
    var date = new Date(dateString);
    var year, month, dayOfMonth;
    year = date.getFullYear();
    month = date.getMonth() + 1;
    dayOfMonth = date.getDate();
    return [year, month, dayOfMonth].join('-');
  };
})();
