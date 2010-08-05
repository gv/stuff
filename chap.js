D = document;
R = .48;
RR4 = 4*R*R;
Q = 1;
N = "http://www.w3.org/2000/svg";

f = D.createElementNS(N, 'svg');
f.style.setProperty("width", "500px", "");
f.style.setProperty("height", "500px", "");
v = f.viewBox.baseVal;
v.x = v.y = 0;
v.height = v.width = 14 * Q;

function P(l, x) {
	l.baseVal.value = x;
}

//f.style.height = "100px";
//f.style.width = "100px";
$(f).
//height(500).width(500).
	appendTo("body");

function C(n) { 
	return f.appendChild(D.createElementNS(N, n));
}

u = [];
function vec(E, v) {
		var p = f.createSVGPoint();
		p.x = E.clientX;
		p.y = E.clientY;
		//console.log(E);
		//console.log(f.getScreenCTM().inverse());
		p = p.matrixTransform(f.getScreenCTM().inverse());
		p.x = v.x - p.x;
		p.y = v.y - p.y;
		return p;
}

function len(p) {
	return Math.sqrt(p.x*p.x+p.y*p.y);
}

F = .1;
//function sb(p, q)

function tick() {
	pair = cont = 0;
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
					
					co = dx*fx + dy*fy;
					if(co < 0) {
						A = fx*fx + fy*fy;
						pl = dx*fy - dy*fx;
						dis = RR4*A - pl*pl;
						if(dis > 0) {
							tt = co + Math.sqrt(dis)/A;
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
			u[i].x += u[i].v.x*t;
			u[i].y += u[i].v.y*t;
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
			
			render();
			p.l.style.setProperty("stroke", "#3ff", "");
			q.l.style.setProperty("stroke", "#3f3", "");
			alert(t);
		}

		T -= t;
	} while(T > 0);

	render();
	setTimeout(tick, 40);
}
						
				
function mm(v) {
	return function(E) {
		p = vec(E, v);
		L = len(p);
		//console.log(s);
		ex = v.x - p.x/L*(R+0.2);
		ey = v.y - p.y/L*(R+0.2);
		P(r.x1, ex - p.x*5);
		P(r.y1, ey - p.y*5);
		P(r.x2, v.x - p.x, ex);
		P(r.y2, v.y - p.y, ey);
		v.l.style.setProperty("stroke", "#f6ff89", "");
		v.l.style.setProperty("stroke-width", .1, "");
		r.style.visibility = "";
		r.style.setProperty("stroke", "#f63589", "");
		r.style.setProperty("stroke-width", .1, "");
		//r.style.stroke = "red";
		//r.style.strokeWidth = .1;
		
		//console.log(r.y2.baseVal.value);
	}
}

function md(v) {
	return function(E) {
		console.log("md");
		v.v = vec(E, v);
		v.v.x *= 3;
		v.v.y *= 3;
		console.log(v.v.x, v.v.y);
		tick();
	}
};

function mo(v) {
	return function(E) {
		r.style.visibility = "hidden";
	}
}
		
		
for(i = 16; i--; ) {
	l = f.appendChild(C('circle'));
	v = {
		x: (i>>1) + 1.5,
				y: i&1 ? 1.5 : 8.5,
		v: {x:0,y:0},
				c: i&1,
				l: l
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
		P(l.cx, v.x * Q);
		P(l.cy, v.y * Q);
		P(l.r, R *Q);
		l.style.setProperty("stroke", "#3589f3", "");
		l.style.setProperty("stroke-width", .01, "");
	}
}

render();



	
	


