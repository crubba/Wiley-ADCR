$(document).ready(function() {
    $("p").hide();
    $("h1").click(function(){
	$(this).nextAll().slideToggle(300);
    });
});