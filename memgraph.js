function MemGraph(node, text) {
	this.wordMap = {};
	this.wordLen = 4;
	this.node = document.getElementById(node);

	this.addText(text);
}

MemGraph.prototype.addText = function(text) {
	var lines = text.split("\n");
	var baseAddr = Infinity;
	var wordLen = this.wordLen;

	for(var i in lines) {
		var line = lines[i];
		var tokens = line.split(new RegExp("[^0-9a-fA-FxX]+"));
		var newBaseAddrName = tokens.shift();
		var newBaseAddr = parseInt(newBaseAddrName);
		while(baseAddr < newBaseAddr && words.length) {
			this.wordMap[baseAddr] = {value: words.shift()};
			baseAddr += wordLen;
		}
		var words = [], baseAddr = newBaseAddr, t;
		while(t = tokens.shift()) {
			words.push(parseInt(t, 16));
		}
	}

	while(words.length) {
		this.wordMap[baseAddr] = {value: words.shift()};
		baseAddr += wordLen;
	}

	this.render();
};	


MemGraph.prototype.render = function() {
	var svgNode = this.node;
	var createSvgNode = function(tagName) {
		return svgNode.appendChild(document.createElementNS(
				"http://www.w3.org/2000/svg", tagName));
	};
				
	this.node.innerHTML = '';
	svgNode = createSvgNode("svg");

	var wordLen = this.wordLen;
	var reprWord = function(n) {
		n = n.toString(16);
		while(n.length < wordLen*2)
			n = '0' + n;
		return n;
	}
	

	var wordHeight = 20;
	var minAddr = Infinity, maxAddr = -Infinity;
	for(var k in this.wordMap) {
		var a = parseInt(k, 10);
		if(a < minAddr)
			minAddr = a;
		if(a > maxAddr)
			maxAddr = a;
	}

	var moveText = function(t, x, y) {
		var l = svgNode.createSVGLength();
		l.value = x;
		t.x.baseVal.appendItem(l);
		var l = svgNode.createSVGLength();
		l.value = y;
		t.y.baseVal.appendItem(l);
	};

	var getY = function(a) {
		return (a - minAddr) * wordHeight / wordLen;
	};

	svgNode.style.height = getY(maxAddr) - getY(minAddr);

	var wordWidth = 150;
	
	for(var k in this.wordMap) {
		var w = this.wordMap[k].value;
		var a = parseInt(k, 10);
		var textTag = createSvgNode("text");
		textTag.appendChild(document.createTextNode(
				reprWord(a) + ": " + reprWord(w)));
		moveText(textTag, 0, getY(a));
		textTag.style.fill = "#F00";

		if(this.wordMap[w]) {
			var srcY = getY(a) - wordHeight / 2;
			var dstY = getY(w) - wordHeight / 2;
			var p = createSvgNode("path");
			textTag.style.fill = "#0F0";
			p.pathSegList.appendItem(p.createSVGPathSegMovetoAbs(0, srcY));
			p.pathSegList.appendItem(p.createSVGPathSegCurvetoQuadraticAbs(
					wordWidth, srcY, 
					wordWidth /2,	srcY - wordHeight / 2));
			p.pathSegList.appendItem(p.createSVGPathSegCurvetoQuadraticAbs(
					wordWidth + Math.abs(a - w), (srcY + dstY)/2, 
					wordWidth + Math.abs(a - w)/2, srcY));
			p.pathSegList.appendItem(p.createSVGPathSegCurvetoQuadraticSmoothAbs(
					wordWidth, dstY));
			/*p.pathSegList.appendItem(p.createSVGPathSegCurvetoQuadraticSmoothAbs(
			0, dstY));*/
			st(p, {"stroke-width": 4, stroke: "#100", fill: "transparent"});
			var back = createSvgNode("rect");
			back.y.baseVal.value = getY(a) - wordHeight + 4;
			back.x.baseVal.value = 0;
			back.height.baseVal.value = wordHeight;
			back.width.baseVal.value = wordWidth;
			svgNode.appendChild(textTag); // bring to front 
		}
	}
		
}
	
function st(e, s) {
	for(k in s) 
		e.style.setProperty(k, s[k], "");
	return s;
}

	
	
		
		
		
