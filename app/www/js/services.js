angular.module('reted.services', [])

.factory('Talks', function($http) {
  var talks = [];

  var all = function() {
    return talks;
  };

  var loadMore = function() {
    var url = '/api/talks';
    if (talks.length) {
      url += '?tid=' + talks.slice(-1)[0].id;
    }
    return $http.get(url)
      .success(function (data) {
        talks = talks.concat(data);
      })
  }

  return {
    loadMore: loadMore,
    all: all
  }
});
