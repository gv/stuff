/*
	defer.js 
	--------
	This library defines operation on "Promise" objects,
	which are variables, whose values can be computed now
	or not computed yet
*/

/*
	Helpers.
*/

function update(d, s) {
	for(var k in s)
		d[k] = s[k];
	return d;
}

function indexOf(a, v, dflt) {
	for(var i in a) 
		if(v == a[i]) return i;
	return dflt || -1;
}

var nop = function() {};

var DEBUG = function(s) {
	console && console.log(s);
};

/* I use the "new,prototype,etc..." mechanism of javascript here
	 to distinguish promised values from regular values
	 using instanceof operator. 

	 Users of this library should not construct 
	 Promise objects themselves. Those values will be returned
	 from defer/DOM api functions. */

notYet = {debug: 'NOT_YET'};

// constructs a deferred value
function Promise(_produce, source) {
	this.v = notYet;
	this.listeners = [];
	this._produce = _produce;
	this.source = source; // circular link
	// source && source.listeners.push(this);
	
	/*
	this.DEBUG_name = 'P ' + 
		(_produce ? _produce.DEBUG_name : '') + '(' + 
		(source ? source.DEBUG_name : '') + ')';
	*/
}

Promise.prototype.set = function(v) {
	if(!(v instanceof Promise)) {
		this.v = v;

		/*
			Clean up possible circular links.
		*/
		delete this.source;
		delete this._produce;
		this.set = function(v) {
			DEBUG('Second set to ' + v);
		};
		
		for(var i in this.listeners) {
			var l = this.listeners[i];
			l.set(l._produce(v));
		}

		delete this.listeners;
	} else if(v.v != notYet) {
		this.set(v.v);
	} else {
		/*
			this._produce returned a Promise.
		*/
		this.v = v;
		delete this.source;
		delete this._produce;

		/*
			Reconnect all listeners to v
		*/
		for(var i in this.listeners) {
			this.listeners[i].source = v;
			v.listeners.push(this.listeners[i]);
		}
			
		this.set = function(v) {
			DEBUG('Second set to ' + v);
		};
	}
	/*
		Also, what should we do if someone tries to 
		1) cancel	already performed evaluation?
		2) set a value twice?
		
		Both situations are kind of not right, and the second is
		very not right.	Maybe we should throw an exception?
	*/
};

Promise.prototype._cancel = function() {
	if(this.source) {
		unlisten(this.source, this._produce);
	}
};

var unlisten = function(p, f) {
	if(!(p instanceof Promise)) {
		// Damage is done
		return false;
	} if(p.v != notYet) {
		return unlisten(p.v, f);
	} else {
		for(var i in p.listeners) {
			if((p.listeners[i] == f) || 
				 (p.listeners[i]._produce == f))  {
				p.listeners.splice(i, 1);

				//alert('remains ' + this.listeners.length + ' ' + this.listeners);
				
				if(!p.listeners.length) { 
					p._cancel();
					delete p._cancel;
				}
				return true;
			}
		}
		return false;
	}
};
	

// parameter order is like f(val)		
function listen(f, thing) {
	/*
		thing is a regular variable
	*/
	if(!(thing instanceof Promise)) 
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
		we can be sure we don't need thing itself!
	*/
	var promise = new Promise(f, thing);
	thing.listeners.push(promise);
	return promise;
}
	

/*
	Core APIs.
	----------
	defer(...) and race(...)
*/

var pair = function(first, second, _construct) {
	_construct = _construct || function(a, b) { return [a,b]; };

	return listen(function(valOfFirst) {
			return listen(function(valOfSecond) {
					return _construct(valOfSecond, valOfSecond);
				}, second);
		}, first);
};
			

/*	Returns function that calls f when all the  arguments are ready.
*/
var defer = function(f, DEBUG_name) {
	var rv = function() {
		if(!arguments.length) {
			DEBUG_name && DEBUG('Calling ' + DEBUG_name + '()');
			return f();
		}

		var args = Array.prototype.slice.call(arguments), vals = [];
		
		var whenArgIsReady = function(val) {
			//alert(vals.length + ' ' + args.length + ' ' + val);
			vals.push(val);
			if(args.length) {
				return listen(whenArgIsReady, args.shift());
			}
			//vals.shift() // remove dummy
			args = null;
			
			DEBUG_name && DEBUG('Calling ' + DEBUG_name + '(' + vals + ')');
			return f.apply(null, vals);
		};

		return listen(whenArgIsReady, args.shift());
	}

	rv.DEBUG_name = DEBUG_name;
	return rv;
};
		
/* Returns the first evaluated result.
	 Unlistens all the rest, which can cause a cancellation.
*/
var race = function() {
	var r = new Promise(), racers = arguments;
	var finishLine = function(val) {
		// Run cancellations.
		DEBUG('race: over, unlistening ' + racers.length);
		for(var i = 0; i < racers.length; i++) {
			unlisten(racers[i], finishLine);
		}
		
		// Run listeners.
		r.set(val);
	};

	for(var i = 0; i < arguments.length; i++) {
		var arg = arguments[i];
		listen(finishLine, arg);
		if(!(arg instanceof Promise)) {
			return arg;
		}
	}

	r._cancel = function() {
		DEBUG('race: cancelled, unlistening ' + racers.length);
		for(var i = 0; i < racers.length; i++) { 
			unlisten(racers[i], finishLine);
		}
		delete r._cancel;
	};
	return r;
};
		
		
		

/*
	DOM APIs
	--------
*/

var hold = function(time, val) {
	val = val || {};
	var r = new Promise();
	setTimeout(function() {
			r.set(val);
		}, time);
	return r;
};

var urlEncode = function(data, head) {
	for(var key in data) {
		head = (head ? head + '&' :	"") + 
		key + '=' + escape(data[key]);
	}
	return head;
};

var __defer_askServer = function(method, url, data) {
	var transport = new XMLHttpRequest(), r = new Promise();
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
		transport.setRequestHeader('Content-Type', 
															 'application/x-www-form-urlencoded;charset=utf-8');
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
	

/*
	This call creates an observer value, which is set on on<evtName> call.
	Default is onclick
*/
var getEvt = /*defer(*/function(l, evtName) {
		evtName = evtName || 'click';
		var hnName = 'on' + evtName;
		
	/* A promise created here is valid until fulfilled or abandoned */
		
	if(l[hnName] && l[hnName]._deferjs_promisedResult)
		return l[hnName]._deferjs_promisedResult;
		
	/* XXX Save other (non getEvt) kinds of handlers */
		
		var r = new Promise();//, oldHandler = l[hnName];

		r._cancel = function() {
			delete l[hnName]; // somehow doesn't work
			l[hnName] = null;
			// XXX maybe that should be for clicks only
			l.disabled = true;
			l.className += ' disabled';
			/* Here we call a hook for custom objects, who need to adjust to
				 making or not making events.	*/
			if(evtName != 'prepareevt')
				l.onprepareevt && l.onprepareevt(evtName);
		};

		l[hnName] = function(arg) {
			var thisFunc = l[hnName];
			delete l[hnName];
			l[hnName] = null;
			l.disabled = true;
			l.className += ' disabled';
			if(evtName != 'prepareevt')
				l.onprepareevt && l.onprepareevt(evtName);
		
			/*
				Client app reacts inside r.set(...) call. So it can set l[hnName] or 
				basically do anything.
			*/
			thisFunc._deferjs_promisedResult.set(arg || window.event);
		};

		l[hnName]._deferjs_promisedResult = r;
		r = null;

		l.disabled = false;
		l.className = l.className && l.className.replace(/disabled/g, '');
		if(evtName != 'prepareevt')
			l.onprepareevt && l.onprepareevt(evtName);
		return l[hnName]._deferjs_promisedResult;
	}/*)*/;
	
    
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
		l.className = className || l.className && l.className.replace(/wait/g, '');
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
