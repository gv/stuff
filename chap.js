D = document;
R = .48;
RR4 = 4*R*R;
Q = 1;
N = "http://www.w3.org/2000/svg";

f = D.createElementNS(N, 'svg');
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
		p.x = E.pageX;
		p.y = E.pageY;
		//console.log(f.getScreenCTM().inverse());
		p = p.matrixTransform(f.getScreenCTM().inverse());
		p.x = v.x - p.x;
		p.y = v.y - p.y;
		return p;
}

function len(p) {
	return Math.sqrt(p.x*p.x+p.y*p.y);
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
		P(r.x2, ex);
		P(r.y2, ey);
		r.style.setProperty("stroke", "#f63589", "");
		r.style.setProperty("stroke-width", .1, "");
		//r.style.stroke = "red";
		//r.style.strokeWidth = .1;
		
		console.log(r.y2.baseVal.value);
	}
}

function md(v) {
	return function(E) {
		v.v = vec(E, v);
		tick();
	}
};

F = .1;

//function sb(p, q)

function tick() {
	pair = y = 0;
	for(i in u) {
		p = u[i];
		v = p.v;
		L = len(v);
		if(L > F) {
			v.x -= v.x/L*F;
			v.y -= v.y/L*F;
		} else {
			v.x = v.y = 0;
		}
	}
		
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
						ff = fx*fx + fy*fy;
						pl = dx*fy - dy*fx;
						dis = RR4*ff - pl*pl;
						if(dis > 0) {
							tt = co + Math.sqrt(dis)/ff;
							if(tt < t) {
								t = tt;
								pair = [p, q];
							}
						}
					}
				}
			}
		}
	} while(T > 0);
						
				
			
		
		
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
	u.push(v);
}

r = C('line');

for(i in u) {
	v = u[i];
	l = v.l;
	P(l.cx, v.x * Q);
	P(l.cy, v.y * Q);
	P(l.r, R *Q);
}



	
	


