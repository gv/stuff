function go(newScale, newUrl) {
	if(newScale < 1)
		newScale = 1;
	location.href = '#' + newScale + '*' + newUrl;
	update();
}

function update() {
	// Applies a hash string
	var u = location.hash.substring(1);
	if(u.indexOf('http') < 0) {
		ind.innerHTML = 'Put image URL in the box and press ENTER';
		return;
	}

	var p = u.split('*'), newScale = scale, newUrl = imgUrl;
	if(p.length >= 2) {
		newScale = parseInt(p.shift(), 10);
		if(newScale < 1)
			newScale = 1;
		newUrl = p.join('*'); // should split() really throw out things?
	} else {
		newUrl = u;
	}

	if(newUrl != imgUrl) {
		scale = newScale;
		loadImg(newUrl);
	}

	if(newScale != scale) {
		scale = newScale;
		updateDisp();
	}
}

function loadBtnClicked() {
	var inp = document.getElementById('url');
	var u = inp.value;
	inp.blur();
	go(scale, u);
}

var data, width, height, scale = 1, imgUrl, energies;

function loadImg(url) {
	ind.innerHTML=  'Loading ' + url + ' ...';
	var img = new Image;
	img.onload = function() {
		ind.innerHTML = img.width + ' x ' + img.height;
		width = refCanv.width = img.width;
		height = refCanv.height = img.height;
		var c = refCanv.getContext('2d');
		c.drawImage(img, 0, 0);

		try {
			data = c.getImageData(0, 0, refCanv.width, refCanv.height);
		} catch(e) {
			alert(e);
		}
		ind.innerHTML = data.data[0];
		imgUrl = url;
		energies = null;
		updateDisp();
	};
	img.src = '/c?' + url;
}

function updateDisp() {
	dispCanv.style.width = dispCanv.width = width;
	dispCanv.style.height = dispCanv.height = height;
	var c = dispCanv.getContext('2d');
	var d = c.createImageData(width, height);
	
	// cache energies
	if(!energies) {
		energies = new Array(d.data.length / 4);
		var pos = d.data.length, epos = energies.length;
		while(pos > 0) {
			pos -= 4;
			energies[epos--] = getEnergy(pos);
		}
	}

	var pos = d.data.length, epos = energies.length, e;
	while(pos > 0) {
		pos -= 4;
		epos--;
		e = scale*energies[epos]/2;
		d.data[pos] = Math.min(255, e);
		d.data[pos+1] = Math.min(Math.max(0, e-256), 255);
		d.data[pos+2] = Math.max(0, e-512);
		d.data[pos+3] = 255; // alpha
	}

	c.putImageData(d, 0, 0);
	ind.innerHTML = 'Multiplier: ' + scale;
}

var refCanv = document.getElementById('ref');
var dispCanv = document.getElementById('display');
var ind = document.getElementById('indicator');

document.onkeydown = function(ev) {
	switch(ev.keyCode) {
	case 13: // enter
	loadBtnClicked();
	break;
	case 65: // A
	go(scale+1, imgUrl);
	break;
	case 90: // Z
	go(scale-1, imgUrl);
	break;
	}
};
	


// image tricks

function getEnergy(pos) {
	var left = pos - 4;
	if(left < 0)
		return 0;
	var upper = pos - width*4;
	if(upper < 0)
		return 0;
	var d = data.data;
	var e = Math.abs(d[pos++] - d[upper++]);
	e += Math.abs(d[pos++] - d[upper++]);
	e += Math.abs(d[pos] - d[upper]);
	pos = left + 4;
	e += Math.abs(d[pos++] - d[left++]);
	e += Math.abs(d[pos++] - d[left++]);
	e += Math.abs(d[pos++] - d[left++]);
	return e;
}

