function update() {
	var u = location.hash.substring(1);
	loadImg(u);
}

function loadBtnClicked() {
	var u = document.getElementById('url').value;
	location.href = '#' + u;
	update();
}

var data, width, height;

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
		//ind.innerHTML = data.data[0];
		updateDisp();
	};
	img.src = url;
}

function updateDisp() {
	dispCanv.style.width = dispCanv.width = width;
	dispCanv.style.height = dispCanv.height = height;
	var c = dispCanv.getContext('2d');
	c.putImageData(data, 0, 0);
}

var refCanv = document.getElementById('ref');
var dispCanv = document.getElementById('display');
var ind = document.getElementById('indicator');

document.onkeydown = function(ev) {
	switch(ev.keyCode) {
	case 13: 
	loadBtnClicked();
	break;
	}
};
	


