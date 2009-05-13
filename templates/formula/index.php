<?php defined( '_VALID_MOS' ) or die( 'Direct Access to this location is not allowed.' ); ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<?php $iso = split( '=', _ISO );
echo '<?xml version="1.0" encoding="'. $iso[1] .'"?' .'>';
?>
<html xmlns="http://www.w3.org/1999/xhtml">
	<head>
	<meta http-equiv="Content-Type" content="text/html; <?php echo _ISO; ?>" />
	<?php 
	mosShowHead(); 
if ( $my->id ) {
	initEditor();
}

$thistemplate = "formula";
?>
<meta http-equiv="Content-Type" content="text/html; <?php echo _ISO; ?>" />
	<link href="<?php echo $mosConfig_live_site; ?>/templates/<?php echo $thistemplate; ?>/css/template_css.css" rel="stylesheet" type="text/css" />
	<link href="<?php echo $mosConfig_live_site; ?>/templates/<?php echo $thistemplate; ?>/css/color_black.css" rel="stylesheet" type="text/css" />
	</head>

	<body>
	<div id="foreword">

	<div class="nav">
	<!--<a href="/"><img src="<?php echo $mosConfig_live_site; ?>/templates/<?php echo $thistemplate; ?>/images/home.gif" /></a>&nbsp;&nbsp;&nbsp;
<a href="mailto:reklama@formulaoflove.ru"><img src="<?php echo $mosConfig_live_site; ?>/templates/<?php echo $thistemplate; ?>/images/mail.gif" /></a>&nbsp;&nbsp;&nbsp;
<a href="mailto:reklama@formulaoflove.ru"><img src="<?php echo $mosConfig_live_site; ?>/templates/<?php echo $thistemplate; ?>/images/star.gif" /></a>&nbsp;&nbsp;&nbsp;
-->

<?php mosLoadModules('top', -2); ?>
</div>

<h1 class="words">
œ–Œ—“»“”“ » œ»“≈–¿ &nbsp;&nbsp;&nbsp;
›À»“Õ€≈ œ–Œ—“»“”“ »  &nbsp;&nbsp;&nbsp;
»Õ“»Ã ƒŒ—”√ &nbsp;&nbsp;&nbsp;
›— Œ–“ ”—À”√»</h1>

</div>


<div id="maindiv">
  <div class="s">
	<div class="s">
  <div id="leftcol">
	<div id="mainmenu">
	<?php
	if (mosCountModules( "left" )) {
		mosLoadModules ( 'left',-3);
	}
	?>
	</div>
	<?php if (mosCountModules( "newsflash" )) { ?>
	<div id="extraleft">
				<?php mosLoadModules ( 'newsflash',-2); ?>
				</div>
						<?php	} ?>
						</div>

						<div id="midpage">
	<div id="maincolumn">
	<div id="content">
	<?php mosMainBody(); ?>
						</div>
						</div><!--maincolumn-->
						</div><!-- midpage-->

						<div id="bottom">
  <?php mosLoadModules ( 'bottom',-3); ?>
						</div>
						<!-- ÚÛÚ Ì‡‰Ó clear ˜ÚÓ·˚ ‚˚ÒÓÚ‡ maindiv .s .s ·˚Î‡ ÌÂ ÏÂÌ¸¯Â Ï‡ÍÒËÏ‡Î¸ÌÓÈ ‚˚ÒÓÚ˚ ‚ÒÂı ÍÓÎÓÌÓÍ -->
						<br style="clear: both;" />
	</div>
	</div>
	</div><!-- maindiv-->

	<div id='textlinks'>
	  <table><tr><td>
	<?php mosLoadModules('toolbar', -2); ?>
  </tr></td></table>
						<br style="clear: both;" />
						</div>

	<div id='banners'>
	<table><tr><td>
	<?php mosLoadModules('banner', -2); ?>
						</td></tr></table>
						</div>

	<div id="footer" >
	<!-- ›ÚÓ ÔÓÍ‡ ÌÂ Ì‡‰Ó -->
  <?php /*include($mosConfig_absolute_path."/templates/".$thistemplate."/includes/footer.php"); */ ?>
  <?php /*include_once( $mosConfig_absolute_path .'/includes/footer.php' ); */ ?>
						<?php mosLoadModules('debug', -1);?>
						</div>

						<script>
							// HACK
						var divs = document.getElementsByTagName('DIV');
for(var i = 0; i < divs.length; i++) {
	var d = divs[i];
	if('sobi2MenuMod' == d.className) {
		var items = d.getElementsByTagName('LI');
		for(var j = 0; j < items.length; j++)
			items[j].style.backgroundPosition = j*10 + 'px 20px';
	}
}
</script>	
</body>
</html>
