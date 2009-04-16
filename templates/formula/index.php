<?php defined( '_VALID_MOS' ) or die( 'Direct Access to this location is not allowed.' ); ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<?php $iso = split( '=', _ISO );
echo '<?xml version="1.0" encoding="'. $iso[1] .'"?' .'>';
?>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; <?php echo _ISO; ?>" />
<?php mosShowHead(); ?>
<?php
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
<div id="maindiv">
  <div class="s">
	<div class="s">
  <div id="leftcol">
	<?php
	if (mosCountModules( "left" )) {
		mosLoadModules ( 'left',-3);
	}
if (mosCountModules( "newsflash" )) {
	?>
	 <div id="extraleft">
		<?php mosLoadModules ( 'newsflash',-2); ?>
		</div>
				<?php
   			}
?>
  </div>

	<div id="midpage">
	<div id="maincolumn">
	<div id="content">
   <a name="content"></a>
   <?php mosMainBody(); ?>
 </div>
</div><!--end maincolumn-->

 </div><!-- midpage-->
 <div id="bottom">
  <?php mosLoadModules ( 'bottom',-3); ?>
</div>
<div id="footer" >
  <?php /*include($mosConfig_absolute_path."/templates/".$thistemplate."/includes/footer.php"); */ ?>
  <?php /*include_once( $mosConfig_absolute_path .'/includes/footer.php' ); */ ?>
  <?php mosLoadModules( 'debug', -1 );?>
</div>
<!-- тут надо clear чтобы высота maindiv .s .s была не меньше максимальной высоты всех колонок -->
<br style="clear: both;" />
</div>
</div>
</div><!-- maindiv-->
</body>
</html>
