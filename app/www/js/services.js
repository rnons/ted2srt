angular.module('reted.services', [])

.factory('API', function($resource) {
  return $resource('/api/', null, {
    getTalks: {
      url: '/api/talks',
      method: 'GET',
      isArray: true
    },

    getTalk: {
      url: '/api/talks/:slug',
      method: 'GET',
      params: {
        slug: '@slug'
      }
    },

    getTalkTranscript: {
      url: '/api/talks/:id/transcripts/txt?lang=en',
      method: 'GET',
      params: {
        id: '@id'
      },
      transformResponse: function(data, headersGetter, status) {
        return {text: data};
      }
    }
  })
})

.factory('Talks', function(API) {
  var talks = [];

  var all = function() {
    return talks;
  };

  var loadMore = function() {
    var params = {}
    if (talks.length) {
      params.tid = talks.slice(-1)[0].id;
    }
    return API.getTalks(params, function (data) {
      talks = talks.concat(data);
    }).$promise;
  }

  return {
    loadMore: loadMore,
    all: all
  }
});
