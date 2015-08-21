angular.module('reted.filters', [])

.filter('seconds', function() {
  return function(input) {
    if (input < 0) return '';
    var inputInt, minutes, seconds;
    inputInt = Math.floor(input);
    minutes = Math.floor(inputInt / 60);
    seconds = inputInt % 60;
    return minutes + ':' + seconds;
  };
});
