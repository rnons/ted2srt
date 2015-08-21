angular.module('reted.services', [])

.factory('API', function($resource) {
  var PREFIX = (document.location.hostname === 'localhost') ? '' : 'http://ted2srt.org';
  return $resource('/api/', null, {
    getTalks: {
      url: PREFIX + '/api/talks',
      method: 'GET',
      isArray: true
    },

    getTalk: {
      url: PREFIX + '/api/talks/:slug',
      method: 'GET',
      params: {
        slug: '@slug'
      }
    },

    getTalkTranscript: {
      url: PREFIX + '/api/talks/:id/transcripts/txt?lang=en',
      method: 'GET',
      params: {
        id: '@id'
      },
      transformResponse: function(data, headersGetter, status) {
        return {text: data};
      }
    },

    getRandomTalk: {
      url: PREFIX + '/api/talks/random',
      method: 'GET',
    },

    search: {
      url: PREFIX + '/api/search',
      method: 'GET',
      isArray: true
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
