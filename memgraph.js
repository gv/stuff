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
			this.wordMap[baseAddr] = words.shift();
			baseAddr += wordLen;
		}
		var words = [], baseAddr = newBaseAddr, t;
		while(t = tokens.shift()) {
			words.push(parseInt(t, 16));
		}
	}

	while(words.length) {
		this.wordMap[baseAddr] = words.shift();
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
	
	for(var k in this.wordMap) {
		var w = this.wordMap[k];
		var a = parseInt(k, 10);
		var textTag = createSvgNode("text");
		textTag.appendChild(document.createTextNode(
				reprWord(a) + ": " + reprWord(w)));
		moveText(textTag, 0, (a - minAddr) * wordHeight / this.wordLen);
		textTag.style.fill = "#F00";
	}
		
}
	
	
	
	
		
		
		
