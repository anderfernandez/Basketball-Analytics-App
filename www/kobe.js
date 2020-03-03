$('.nav-tabs a').eq(2).on('show.bs.tab', function(){
  $(".nav-tabs li a").addClass("kobe_tab");
  $(".nav-tabs li a").addClass("kobe_tab");
  $(".nav-tabs").addClass("kobe_nav-tab");
  $(".tabbable").addClass("kobe_tabbable");
  $("body").addClass("kobe_body");
  //$(".nav-tabs li a").addClass("kobe_tab");
  //alert('New tab will be visible now!');
});

$('.nav-tabs a').eq(1).on('show.bs.tab', function(){
  $(".nav-tabs li a").removeClass("kobe_tab");
  $(".nav-tabs li a").eq(0).removeClass("kobe_tab");
  $(".nav-tabs").removeClass("kobe_nav-tab");
  $(".tabbable").removeClass("kobe_tabbable");
  $("body").removeClass("kobe_body");
  //$(".nav-tabs li a").addClass("kobe_tab");
  //alert('New tab will be visible now!');
});


$('.nav-tabs a').eq(0).on('show.bs.tab', function(){
  $(".nav-tabs li a").eq(1).removeClass("kobe_tab");
  $(".nav-tabs li a").eq(0).removeClass("kobe_tab");
  $(".nav-tabs").removeClass("kobe_nav-tab");
  $(".tabbable").removeClass("kobe_tabbable");
  $("body").removeClass("kobe_body");
  //$(".nav-tabs li a").addClass("kobe_tab");
  //alert('New tab will be visible now!');
});




//alert('New tab will be visible now!');

//function cambio(){
//if(document.querySelector(".active a").getAttribute("data-value") == "kobe"){
	// Cambiar el fondon de los tabs
//	document.querySelectorAll(".nav-tabs li a")[0].className += "tabs-kobe";
//	document.querySelectorAll(".nav-tabs li a")[1].className += "tabs-kobe";
//	alert("kobe_tab");
//}
//}
//document.querySelectorAll(".nav-tabs li a")[2].addEventListener("click", cambio);
