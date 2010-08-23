D = document;
R = .48;
RR4 = 4*R*R;
Q = 200;
N = "http://www.w3.org/2000/svg";
TB = LB = 2;
BB = RB = 12;
ZB = 40;

function cancel(e){
	if(window.event)
		e=window.event;
	if(e.preventDefault)
		e.preventDefault(),	e.stopPropagation();
	else
		e.returnValue=e.cancelBubble=true;
}

function fr(l) {
	return F.appendChild(l);
}

function C(n) { 
	return fr(D.createElementNS(N, n));
}

F = document.body;
F = C('svg');
v = F.viewBox.baseVal;
v.x = 2* Q;
v.y = 1 *Q;
v.height = 14 * Q;
v.width = 10 * Q;

function P(l, x) {
	l.baseVal.value = x * Q;
	return x;
}

function st(e, s) {
	for(k in s) 
		e.style.setProperty(k, s[k], "");
	return s;
}

	var r = C("rect");
	P(r.y, P(r.x, 2));
	P(r.width, P(r.height, 10));
	P(r.rx, P(r.ry, .1));
	st(r, {"stroke-width": .05,
				"stroke": "#100",
				fill: "#fff",
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
		st(r, {fill: (x+y)%2 ? "#fff" : "#000"});
	}
		


function vec(E, v) {
	var p = F.createSVGPoint();
	p.x = E.clientX;
	p.y = E.clientY;
	//console.log(E);
	//console.log(F.getScreenCTM().inverse());
	p = p.matrixTransform(F.getScreenCTM().inverse());
	p.x = v.x - p.x/Q;
	p.y = v.y - p.y/Q;
	return p;
}

function len(p) {
	return Math.sqrt(p.x*p.x+p.y*p.y);
}

function pck(a) {
	return a[Math.floor(Math.random() * a.length)];
}

function mv() {
	console.log(cc);
	bs = [[],[]];
	for(i in u)
		if(!u[i].z)
			bs[u[i].c].push(u[i]);
	if(bs[0].length) {
		if(bs[1].length) {
			if(!cc) {
				b = pck(bs[0]);
				t = pck(bs[1]);
				v = {x: t.x - b.x, y: t.y - b.y};
				l = len(v);
				v.x *= R/l;
				v.y *= R/l;
				ph(b, v);
			}
		} else {
			cc = 0;
			sn("You lose");
		}
	} else {
		if(bs[1].length)
			sn("You win"), cc = 1;
		else 
			sn("Draw");
	}
}

F = .1;
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

		if(p.z && (p.z < ZB))
			cont = 1;
	}
	
	if(!cont) {
		mg = 0;
		cc ^= 1;
		mv();
		return;
	}
	
	setTimeout(tick, 40);

	T = 1;
	do {
		pair = 0;
		t = T;
		for(i = 0; i < u.length; i++) {
			p = u[i];
			if(p.z) 
				continue;
			v = p.v;
			for(j = i+1; j < u.length; j++) {
				q = u[j];
				if(q.z)
					continue;
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
							if(tt >= 0 && tt < t) 
								t = tt,	pair = [p, q];
						}
					}
				}
			}
		}

		for(i in u) {
			p = u[i];
			if(p.v.x || p.v.y) 
				p.x += p.v.x*t, p.y += p.v.y*t,	p.rd = 0;
			p.z ? 
				(p.z += 1,	p.rd = 0):
				(p.y < TB || p.y > BB || p.x > RB || p.x < LB) && 
				(p.z = 1, F.insertBefore(p.l, F.firstChild));
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
		}

		T -= t;
	} while(T > 0);

	rr();
}

function aim() {
	if(lp) {
		st(r, {visibility: "", "stroke": "#f63589", "stroke-width": .1 * Q});
		v = lp.lv;
		L = len(v);
		ex = lp.x - v.x/L*(R+0.2);
		ey = lp.y - v.y/L*(R+0.2);
		P(r.x1, ex - v.x*5);
		P(r.y1, ey - v.y*5);
		P(r.x2, ex);
		P(r.y2, ey);
	} else 
		st(r, {visibility: "hidden"});
}
				
function mm(p) {
	return function(E) {
		if(mg || cc != p.c)
			return;
		p.lv = vec(E, p);
		lp = p;
		aim();
	}
}

function ph(p, v) {
	if(v.x || v.y)
		v.x *= 6, v.y *= 6,	p.v = v, 	mg = 1, tick();
}
	
lp = 0;
F.onmousedown = function() {
	console.log("oo");
	if(lp){
		ph(lp, lp.lv);
	}
};
						
function md(p) {
	return function(E) {
		if(mg || cc != p.c)
			return;
		console.log("md");
		ph(lp, lp.lv);
	}
};

function mo(v) {
	return function(E) {
		console.log('out');
		//lp = 0;
	}
}
		
u = [];
for(i = 16; i--; ) {
	l = C('circle');
	P(l.r, R);
	st(l, {stroke: "#888", fill: i&1 ? "#fff": "#000", 
				"stroke-width": .05*Q});
	v = {
		c: i&1,
		l: l
	};
	l.onmousemove = mm(v);
	//l.onmousedown = md(v);
	l.onmouseout = mo(v);
	u.push(v);
}

function go() {
	st(ST, st(SN, {visibility: "hidden"}));
	mg = 0;
	for(i in u) {
		p = u[i];
		p.x = (i>>1) + 3.5;
		p.y = i&1 ? 3.5 : 10.5;
		p.rd = 0;
		p.z = 0;
		p.v = {x:0, y:0};
		fr(p.l);
	}
	rr();
	mv();
}

r = C('line');

function rr() {
	for(i in u) {
		p = u[i];
		if(p.rd)
			continue;
		l = p.l;
		P(l.cx, p.x);
		P(l.cy, p.y);
		P(l.r, R*20/(p.z+20));
		p.rd = 1;
	}
}

function PR(l,x,y,w,h) {
	P(l.x, x);
	P(l.y, y);
	P(l.width,w);
	P(l.height, h);
}

SN = C("rect");
P(SN.rx, P(SN.ry, .6));
P(SN.x, 2.5);
P(SN.y, 4);
P(SN.width, 9);
P(SN.height, 6);
st(SN, {fill: "#8ac", opacity: .9, stroke: "#246", "stroke-width": 0.1*Q});
SN.onmousedown = function(e) {
	cancel(e);
	go();
};

ST = C("text");

function T(a, v) {
	l = F.createSVGLength();
	l.value = v *Q;
	a.baseVal.appendItem(l);
	return v;
}
T(ST.x, 4.5);
T(ST.y, 7);
st(ST, {fill: "#fff", "font-size": Q+"px"});

SU = 0;
function sn(t) {
	fr(SN);
	fr(ST);
	SU&&ST.removeChild(SU)
	SU = ST.appendChild(D.createTextNode(t));
	st(ST, st(SN, {visibility: ""}));
}

cc = 1;
go();



	
	


