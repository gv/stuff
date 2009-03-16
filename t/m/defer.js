/*
	defer.js 
	--------
	This library defines operation on "Future" objects,
	which are variables, whose values can be computed now
	or not computed yet
*/

function indexOf(a, v, dflt) {
	for(var i in a) 
		if(v == a[i]) return i;
	return dflt || -1;
}

/* I use the "new,prototype,etc..." mechanism of javascript here
	 to distinguish future values from regular values
	 using instanceof operator. */

/* Users of this library should not construct 
	 future values themselves. Those values will be returned
	 from defer/DOM api functions. */

notYet = {debug: 'NOT_YET'};

var nop = function() {};

// constructs a deferred value
function Future() {
	this.v = notYet;
	this.listeners = [];
}

// XXX make set a bound method maybe
Future.prototype.set = function(v) {
	this.v = v;
	for(var i in this.listeners) {
		var l = this.listeners[i];
		/*
			XXX
			If v is a Promise, maybe we could just hang our listeners on it
			instead of creating potentially huge promise chain.
		*/
		l.r.set(listen(l.act, v));
	}
	/*
		Cleanup possible circular links.
	*/
	this.listeners = [];
	delete this.cancel;
	/*
		Also, what should we do if someone tries to 
		 1) cancel	already performed evaluation?
		 2) set a value twice?

		Both situations are kind of not right, and the second is
		very not right.	Maybe we should throw an exception?
	*/
};

Future.prototype.cancel = nop;

Future.prototype.unlisten = function(f) {
	for(var i in this.listeners) {
		if(this.listeners[i].act == f)  {
			this.listeners.splice(i, 1);
			//alert('remains ' + this.listeners.length + ' ' + this.listeners);
			if(!this.listeners.length) { 
				this.cancel();
				delete this.cancel;
			}
			return true;
		}
	}
	return false;
};
	

// parameter order is like f(val)		
function listen(f, thing) {
	/*
		thing is a regular variable
	*/
	if(!(thing instanceof Future)) 
		return f(thing);
	/*
		thing is a fulfilled promise
	*/
	if(thing.v != notYet)
		return listen(f, thing.v);
	/*
		Value of thing is not known yet, so we give client a promise 
		that we hand him f(thing) when thing is available. 
		
		It seems we should give them a link to us to, so they can let us know they 
		don't need f(thing) more, so if noone else needs g(thing) or eatConcrete(thing),
		we can be almost sure we don't need thing itself!
	*/
	var promise = new Future();
	promise.cancel = function() {
		thing.unlisten(f);
	};
	thing.listeners.push({act: f, r: promise});
	return promise;
}
	

/*
	Core APIs.
	----------
	defer(...) and race(...)
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
		
/* Returns the first evaluated result.
	 Unlistens all the rest, which can cause a cancellation.
*/
var race = function() {
	var r = new Future(), racers = arguments;
	var finishLine = function(val) {
		// Run cancellations.
		//alert('unlistening ' + racers.length);
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

var urlEncode = function(data, head) {
	for(var key in data) {
		head = (head ? head + '&' :	"") + 
		key + '=' + escape(data[key]);
	}
	return head;
};

var __defer_askServer = function(method, url, data) {
	var transport = new XMLHttpRequest(), r = new Future();
	transport.onreadystatechange = function() {
		switch(transport.readyState) {
			// XXX handle other states
		case 4: 
		var answer = eval('(' + transport.responseText + ')');
		r.set(answer);
		}
	};

	if('GET' == method) {
		transport.open('GET', url + (data ? ('?' + urlEncode(data)) : ''));
		transport.send(null);
	} else {
		transport.open('POST', url);
		transport.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded;charset=utf-8');
		transport.send(data ? urlEncode(data) : "")
	}
	
	return r;
};

var askServer = defer(function(url, data) {
		return __defer_askServer('GET', url, data);
	});
	
	
var send = defer(function(url, data) {
		return __defer_askServer('POST', url, data);
	});
	

// default is onclick
var getEvt = function(l, evtName) {
	evtName = 'on' + (evtName || 'click');
	var r = new Future();

	r.cancel = function() {
		l.disabled = true;
		l.className = l.className.replace(/disabled/g, '');
		delete l[evtName];
	};

	l[evtName] = function(arg) {
		delete l[evtName];
		l.disabled = true;
		l.className = l.className.replace(/disabled/g, '');
		r.set(arg || window.event);
	};

	l.disabled = false;
	l.className += ' disabled';
	return r;
};
	
    
// helpers

var L = defer(function(id) {
		return ('string' == typeof id) ? document.getElementById(id) : id;
	});


var stick = defer(function(parent, tagName, className, body) {
		var r = L(parent).appendChild(document.createElement(tagName));
		r.className = className;
		if(body) r.innerHTML = body;
		r.disabled = true;
		return r;
	});

var clearFloats = function(l) {
	var c = stick(l, 'DIV');
	c.style.clear = 'both';
	return c;
}

var __defer_indicate = defer(function(l, body, className) {
		l.innerHTML = body;
		l.className = className || l.className.replace(/wait/g, '');
	});

var indicate = function(l, body, className) {
	l = L(l);
	l.className += ' wait';
	__defer_indicate(l, body, className);
	return l;
};
	

/*
	This function provides generic algorithm for updating an array of controls 
	to reflect a list of values.
	We do not rebuild every control, so check box states/focuses/other stuff
	remains untouched. However, insertions and other order changes can make it behave 
	irrationally.
*/
var updateControls = defer(function(items, 
														controls, 
														_buildControl, 
														_updateControl, 
														_rmControl) {
		for(var i = 0; i < items.length; i++) {
			var item = items[i], control = controls[i];
			if(!control) {
				control = _buildControl();
				controls.push(control);
			}
			_updateControl(control, item);
		}

		while(controls.length > items.length) {
			(_rmControl || 
			 function(c) { c.l.parentNode.removeChild(c.l); })(controls.pop());
		}
													 });

/*
	What kind of functional library would we be if we did not include these?
*/

var map = defer(function(list, f) {
	var r = [];
	for(var i in list) r.push(f(list[i]));
	return r;
	});

var filter = defer(function(list, f) {
		var r = [];
		for(var i in list) 
			if(f(list[i]))
				r.push(list[i]);
		return r;
	});

var identity = defer(function(x) {
		return x;
	});

var first = defer(function(o) {
		for(var k in o)
			return o[k];
	});

/*
	Operators
*/

var prop = function(key) {
	return defer(function(obj) {
			return obj[key];
		});
};
