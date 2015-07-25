angular.module('reted.directives', [])

.directive('focusMe', function($timeout) {
  return function(scope, element) {
    $timeout(function() {
      element[0].focus();
    });
  };
})

