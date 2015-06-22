angular.module('reted.services', [])

.factory('Talks', function($http) {
  var talks = [];

  var fetch = function() {
    return $http.get('/api/talks')
      .success(function (data) {
        talks = data;
      })
  };

  var all = function() {
    return talks;
  }

  return {
    fetch: fetch,
    all: all
  }
});
