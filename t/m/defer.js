/*
	defer.js 
	--------
	This library defines operation on "Future" objects,
	which are variables, whose values can be computed now
	or not computed yet
*/

/* I use the "new,prototype,etc..." mechanism of javascript here
	 to distinguish future values from regular values
	 using instanceof operator. */

/* Users of this library should not construct 
	 future values themselves. Those values will be returned
	 from defer/DOM api functions. */

function indexOf(a, v, dflt) {
	for(var i in a) 
		if(v == a[i]) return i;
	return dflt || -1;
}

// constructs a deferred value
function Future() {
	this.listeners = [];
}

Future.prototype.listen = function(act) {
	if(typeof this.v == 'undefined') {
		var r = new Future();
		this.listeners.push({act: act, r: r});
		return r;
	} else {
		// Could val also be a future?
		//return listen(this.v, act);
		return act(this.v);
	}
};

// XXX make set a bound method maybe
Future.prototype.set = function(v) {
	this.v = v;
	for(var i in this.listeners) {
		var l = this.listeners[i];
		// see question above
		// l.r.set(listen(l.act, v));
		l.r.set(l.act(v));
	}
	this.listeners = [];
};

Future.prototype.cancel = function() {};

Future.prototype.unlisten = function(f) {
	this.listeners.splice(indexOf(this.listeners, f, this.listeners.length), 1);
	this.listeners.length || this.cancel();
};
	

// parameter order is like f(val)		
function listen(f, val) {
	return (val instanceof Future) ?
		val.listen(f):
		f(val);
}
	

/*
	Core APIs.
	----------
	defer(...) and  race()
*/

var pair = function(first, second, _construct) {
	_construct = construct || 
	function(a, b) { return [a,b]; };

	return listen(function(valOfFirst) {
			return listen(function(valOfSecond) {
					return _construct(valOfSecond, valOfSecond);
				}, second);
		}, first);
};
			

// receives an array of Futures or regular values
// returns Future of array of regular values
var __list = function(lst) {
	if(lst.length) {
		var tail = lst.slice(1);
		return listen(function(first) {
				

			}, lst[0]);
	} else
		return [];
};
		
/*
var defer = function(f) {
	return function() {
		var n = arguments.length;
		if(!n--) return f();
		return listen(function(val1) {
				if(!n--) 
					return f(val1);
				return listen(function(val2) {
						if(!n--)
							return f(val1, val2)
*/

/*
var defer = function(f) {
	return function() {
		var origLen = arguments.length, vals = [];
		return origLen ?
		listen(function(val) {
				vals.push(val);
				if(vals.length == origLen) {
					return f.apply(null, vals);
				}
*/

/*	Returns function that calls f when all the  arguments are ready.
*/
var defer = function(f) {
	return function() {
		var args = Array.prototype.slice.call(arguments), vals = [];
		
		var p = function(val) {
			//alert(vals.length + ' ' + args.length + ' ' + val);
			vals.push(val);
			if(args.length) {
				return listen(p, args.shift());
			}
			vals.shift()
			return f.apply(null, vals);
		};

		return p(null); // dummy
	}
};
		
/*
var defer = function(f) {
	var res = function(first) {
		if(!arguments.length) return f();

		var shifted = defer(function() {
			return f.apply(null, [first].concat(Array.prototype.slice.call(arguments)));
			}),	tail = Array.prototype.slice.call(arguments, 1);

		return listen(function(firstVal) {
				return shifted

			}, first);
	};
	return res;
};
*/
	
// Returns the first evaluated result.
// Unlistens all the rest, which can cause a cancellation.
var race = function() {
	var r = new Future(), racers = arguments;
	var finishLine = function(val) {
		// Run cancellations.
		for(var i = 0; i < racers.length; i++) {
			if(racers[i] instanceof Future) {
				racers[i].unlisten(finishLine);
			}
		}
		
		// Run listeners.
		r.set(val);
	};

	for(var i = 0; i < arguments.length; i++) {
		var arg = arguments[i];
		listen(finishLine, arg);
		if(!(arg instanceof Future)) {
			return arg;
		}
	}
	return r;
};
		
		
		

/*
	DOM APIs
	--------
*/

var hold = function(time, val) {
	val = val || {};
	var r = new Future();
	setTimeout(function() {
			r.set(val);
		}, time);
	return r;
};

// for POST
// var informServer = function

var askServer = function(url) {
	var method = 'GET';
	var transport = new XMLHttpRequest(), r = new Future();
	transport.onreadystatechange = function() {
		switch(transport.readyState) {
			// XXX handle other stuff
		case 4: 
		var answer = eval('(' + transport.responseText + ')');
		r.set(answer);
		}
	};
	transport.open(method, url);
};

var getEvt = function(l, evtName) {
	evtName = 'on' + evtName;
	var r = new Future(), prevHandlerFunction = l[evtName];
	l[evtName] = function(arg) {
		r.set(arg);
		l[evtName] = prevHandlerFunction;
	};
	return r;
};
	
    
// helpers

var L = defer(function(id) {
		return ('string' == typeof id) ? document.getElementById(id) : id;
	});


var addEl = defer(function(parent, tagName, className) {
		var r = L(parent).appendChild(document.createElement(tagName));
		r.className = className;
		return r;
	});


    
	
	
