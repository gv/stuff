
var plus = function(left) {
    return function(right) {
	return left + right;
    };
};

var sum = plus(121)(232);

var ternary = function(left) {
    return left ? 
    function(middle) {
	return function() {
	    return middle;
	}
    } :
    function() {
	return function(right) {
	    return right;
	}
    };
};

var oddity = ternary(sum % 2)('odd')('even');

// теперь как бы сделать чтобы по true запускалась одна ветка а по false другая 

	    