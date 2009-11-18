function Loc(h) {
	h = h.split('*');
	this.xScale = h.shift() || 1;
	this.url = h.join('*');
}

Loc.prototype.toString = function() {
	return this.xScale + '*' + this.url;
};

function Img(bs) {
	this.bs = bs;
	refCanv.height = bs.height;
	refCanv.width = bs.width;
	var c = refCanv.getContext('2d');
	c.drawImage(bs, 0, 0);

	try {
		this.imData = c.getImageData(0, 0, bs.width, bs.height);
	} catch(e) {
		alert(e);
	}
}

Img.prototype.getEnergies = function() {
	if(!this.energies) {
		var d = this.imData.data;
		var ee = new Array(d.length / 4), i = 1, p = 4, end;
		ee[0] = 0; // xxx
		for(; i < this.bs.width; i++) {
			ee[i] = 0; // xxx
			p += 4;
		}

		end = ee.length; 
		var upper = 0, left = p - 4, e;
		for(; i < end; i++) {
			e = Math.abs(d[p++] - d[left++]);
			e += Math.abs(d[p++] - d[left++]);
			e += Math.abs(d[p] - d[left++]);
			p = ++left;

			e += Math.abs(d[p++] - d[upper++]);
			e += Math.abs(d[p++] - d[upper++]);
			e += Math.abs(d[p++] - d[upper++]);
			upper++;
			p++;
			ee[i] = e;
		}
		this.energies = ee;
	}
	return this.energies;
};


Img.prototype.getSums = function() {
	if(!this.sums) {
		var ee = this.getEnergies();
		var ss = new Array(ee.length), i = 0, s;
		for(; i < this.bs.width; i++) 
			ss[i] = ee[i];

		ss[i] = Math.min(ee[0], ee[1]);
		i++;
		
		var upper = 1;
		for(; i < ss.length; i++) {
			ss[i] = ee[i] + Math.min(ss[upper-1], ss[upper], ss[upper+1]);
			upper++;
		}
		this.sums = ss;
	}
	return this.sums;
};

var curImg;
function loadImg(url, _handle) {
	if(curImg && curImg.bs.src == url)
		return _handle(curImg);

	var im = new Image();
	im.onload = function() {
		ind.innerHTML = im.width + 'x' + im.height;
		curImg = new Img(im);
		_handle(curImg);
	};

	ind.innerHTML = 'Loading ' + url + ' ...'; 
	im.src = url;
}
		
		

function render() {
	if(!loc.url) {
		ind.innerHTML = 'give me url';
		return;
	}
	
	loadImg('/c?' + loc.url, function(img) {
			var scaledWidth = Math.floor(img.bs.width * loc.xScale);
 			dispCanv.width = dispCanv.style.width = 
				img.bs.width;
			dispCanv.height = dispCanv.style.height = img.bs.height;
			
			var c = dispCanv.getContext('2d'), s = '';
			var dest = c.createImageData(img.bs.width, dispCanv.height);
			var sums = img.getSums(), d = dest.data;
			var src = img.imData.data;

			var excessColCnt = img.bs.width - scaledWidth;
			ind.innerHTML = excessColCnt;
			var lastRowStart = sums.length - img.bs.width;

			s = sums.length + ': ' + sums.join(' ');

			//var skip = new Array(excessColCnt * dispCanv.height), 
			//	skipCnt = 0;

			var visMap = new Array(sums.length);

			for(var visLvl = 1; visLvl <= excessColCnt; visLvl++) {
				var minInd = Infinity;
				for(var i = sums.length - 1; i >= lastRowStart; i--) {
					if(!visMap[i]) {
						if(sums[i] < minSum) {
							minSum = sums[i];
							minInd = i;
						}
					}
				}
				
				s += minInd + '=' + minSum + ' ';

				//sums[minInd] = Infinity;
				var rowStart = lastRowStart;
				do {
					visMap[minInd] = visLvl;
					minInd -= img.bs.width;
					
					//rowStart -= img.bs.width;
					if(minInd < 0)
						break;

									
					if(sums[minInd+1] < sums[minInd])
						minInd++;
					if(sums[minInd+1] < sums[minInd])
						minInd++;
					sums[minInd] = Infinity;
					skip[skipCnt++] = minInd;
					minInd -= (img.bs.width + 1);
				} while(1);
			}
					
				
			//for(var i = 0; i < skip.length; i++)
			//	skip[i] = Math.floor(Math.random() * sums.length);

			
			// default sort() is lexicographical
			skip.sort(function(l,r) {
					if(l < r)
						return -1;
					if(l > r)
						return 1;
					return 0;
				});

			//ind.innerHTML = skip.join(' ');
			skip.push(Infinity);

			for(var i = 0, j = 0, sp = 0, dp = 0; i < sums.length; i++) {
				if(skip[j] > i) {
					d[dp++] = sums[i];
					d[dp++] = sums[i];
					d[dp++] = sums[i];
					d[dp++] = 255;
					/*d[dp++] = src[sp++];
					d[dp++] = src[sp++];
					d[dp++] = src[sp++];
					d[dp++] = src[sp++];*/
				} else {
					//s += i + ":" + j + '=' + skip[j] + '; ';
					j++;
					sp += 4;
					d[dp++] = 0;
					d[dp++] = 255;
					d[dp++] = 0;
					d[dp++] = 255;
					}
			}
			
			dbgInd.innerHTML = s;
			c.putImageData(dest, 0, 0);

			location.href = '#' + loc.toString();
			//ind.innerHTML = 'Press keys: a s'
		});
	
}

function loadBtnClicked() {
	loc.url = document.getElementById('url').value;
	loc.xScale = 1;
	render();
}
			
var refCanv = document.getElementById('ref');
var dispCanv = document.getElementById('display');
var ind = document.getElementById('indicator');
var dbgInd = document.getElementById('dbgInd');

document.onkeydown = function(ev) {
	switch(ev.keyCode) {
	case 13: // enter
	loadBtnClicked();
	break;

	case 65: // A
	if(loc.xScale > 0.1) { 
		loc.xScale -= 0.1;
		render();
	}
	break;

	case 90: // Z
	if(loc.xScale < 0.9) { 
		loc.xScale += 0.1;
		render();
	}
	break;
	}
};
	
var h = location.hash.substring(1);
var loc = new Loc(h);
render();

