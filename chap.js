D = document;
R = .48;
RR4 = 4*R*R;
Q = 200;
N = "http://www.w3.org/2000/svg";
TB = LB = 2;
BB = RB = 10;

function C(n) { 
	return f.appendChild(D.createElementNS(N, n));
}

f = document.body;
f = C('svg');
v = f.viewBox.baseVal;
v.x = v.y = 0;
v.height = v.width = 14 * Q;

function P(l, x) {
	l.baseVal.value = x * Q;
	return x;
}

function st(e, s) {
	for(k in s) {
		e.style.setProperty(k, s[k], "");
	}
}

(function() {
	var r = C("rect");
	P(r.y, P(r.x, 2));
	P(r.width, P(r.height, 10));
	P(r.rx, P(r.ry, .1));
	st(r, {"stroke-width": .05,
				"stroke": "#100",
				fill: "#fff"
				});
				
	var s = C("rect");
	P(s.y, P(s.x, 3));
	P(s.width, P(s.height, 8));
	st(s, {fill: "#000"});
	for(i = 64; i;) {
		r = C("rect");
		P(r.width, P(r.height, 1));
		x = --i % 8;
		y = (i-x)/8;
		P(r.x, x + 3);
		P(r.y, y + 3);
		P(r.rx, P(r.ry, 0.12));
		st(r, {fill : (x+y)%2 ? "#fff" : "#000"});
	}
})();
		


u = [];
function vec(E, v) {
		var p = f.createSVGPoint();
		p.x = E.clientX;
		p.y = E.clientY;
		//console.log(E);
		//console.log(f.getScreenCTM().inverse());
		p = p.matrixTransform(f.getScreenCTM().inverse());
		p.x = v.x - p.x/Q;
		p.y = v.y - p.y/Q;
		return p;
}

function len(p) {
	return Math.sqrt(p.x*p.x+p.y*p.y);
}

F = .1;
//function sb(p, q)

function tick() {
	cont = 0;
	for(i in u) {
		p = u[i];
		v = p.v;
		L = len(v);
		if(L > F) {
			cont = 1;
			v.x -= v.x/L*F;
			v.y -= v.y/L*F;
		} else {
			v.x = v.y = 0;
		}
	}
	
	if(!cont)
		return;
		
	T = 1;
	do {
		pair = 0;
		t = T;
		for(i = 0; i < u.length; i++) {
			p = u[i];
			v = p.v;
			for(j = i+1; j < u.length; j++) {
				q = u[j];
				w = q.v;
				
				fx = w.x - v.x;
				fy = w.y - v.y;
				if(fx || fy) {
					dx = q.x - p.x;
					dy = q.y - p.y;
					
					k = dx*fx + dy*fy;
					if(k < 0) {
						a = fx*fx + fy*fy;
						c = dx*dx + dy*dy - RR4;
						dis = k*k - a*c;
						if(dis > 0) {
							tt = (-k - Math.sqrt(dis))/a;
							if(tt >= 0 && tt < t) {
								t = tt;
								pair = [p, q];
							}
						}
					}
				}
			}
		}

		for(i in u) {
			p = u[i];
			p.x += p.v.x*t;
			p.y += p.v.y*t;
			if(p.z)
				p.z += 1;
			if(p.y < TB || p.y > BB || p.x > RB || p.x < LB)
				p.z = 1;
		}

		if(pair) {
			p = pair[0], q = pair[1];
			var ex = p.x - q.x;
			var ey = p.y - q.y;
			var e2 = ex*ex + ey*ey;
			var m0 = (ex*p.v.x+ey*p.v.y)/e2;
			var m1 = (ex*q.v.x+ey*q.v.y)/e2;
			var change = 0.75*(m1-m0);
			p.v.x += ex*change;
			p.v.y += ey*change;
			q.v.x -= ex*change;
			q.v.y -= ey*change;
			
			/*render();
			st(p.l, {"stroke": "#3ff", "stroke-width": 0.1*Q});
			st(q.l, {"stroke": "#3ff", "stroke-width": 0.1*Q});*/
			//alert(t);
		}

		T -= t;
	} while(T > 0);

	render();
	setTimeout(tick, 40);
}

function point() {
	if(lp) {
		st(r, {visibility: "", "stroke": "#f63589", "stroke-width": .1 * Q});
		v = lp.lv;
		console.log(v.x, v.y);
		L = len(v);
		ex = lp.x - v.x/L*(R+0.2);
		ey = lp.y - v.y/L*(R+0.2);
		P(r.x1, ex - v.x*5);
		P(r.y1, ey - v.y*5);
		P(r.x2, ex);
		P(r.y2, ey);
	} else {
		st(r, {visibility: "hidden"});
	}
}
				
function mm(p) {
	return function(E) {
		p.lv = vec(E, p);
		lp = p;
		point();
	}
}

function pu(p, v) {
	v.x *= 3;
	v.y *= 3;
	p.v = v;
	console.log(p.v.x, p.v.y);
	tick();
}
	
lp = 0;
f.onmousedown = function() {
	console.log("oo");
	if(lp){
		pu(lp, lp.lv);
	}
};
						
function md(p) {
	return function(E) {
		console.log("md");
		pu(lp, lp.lv);
	}
};

function mo(v) {
	return function(E) {
		console.log('mo');
		//lp = 0;
	}
}
		
		
for(i = 16; i--; ) {
	l = f.appendChild(C('circle'));
	v = {
		x: (i>>1) + 3.5,
		y: i&1 ? 3.5 : 10.5,
		v: {x:0,y:0},
		c: i&1,
		l: l,
		z: 0
	};
	l.onmousemove = mm(v);
	l.onmousedown = md(v);
	l.onmouseout = mo(v);
	u.push(v);
}

r = C('line');

function render() {
	for(i in u) {
		v = u[i];
		l = v.l;
		P(l.cx, v.x);
		P(l.cy, v.y);
		P(l.r, R);
		l.style.setProperty("stroke", "#888", "");
		l.style.setProperty("stroke-width", .05*Q, "");
	}
}

render();



	
	


