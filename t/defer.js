// I use the "new,prototype,etc..." mechanism of javascript here
// to distinguish future values from regular values
// using instanceof operator.

// Users of this library should not construct 
// future values themselves. Those values will be returned
// from defer/DOM api functions.

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

Future.prototype.set = function(v) {
	this.v = v;
	for(var i in this.listeners) {
		var l = this.listeners[i];
		// see question above
		// l.r.set(listen(l.act, v));
		l.r.set(l.act(v));
	}
};

/*function ready(val) {
	if(val instanceof Future) {*/

// parameter order mimicks f(val)		
function listen(f, val) {
	return (val instanceof Future) ?
		val.listen(f):
		f(val);
}
	
		

// returns function that calls f when f's argument is ready
var dfr = function(f) {
	return function(arg) {
		return listen(f, arg);
	}
};
	


// ------------------ apis ---------------------------------------

// for POST
// var informServer = function

var askServer = function(url) {
	var method = 'GET';
	var transport = new XMLHttpRequest();
	transport.onreadystatechange = function() {
		switch(transport.readyState) {
			// XXX handle other stuff
		case 4:
	
