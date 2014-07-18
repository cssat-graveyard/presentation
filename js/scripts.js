// Next and previous buttons
$( document ).ready(function() {
		$("#next").click(function () {
		   impress().next();
		});

		$("#prev").click(function () {
		   impress().prev();
		});
});

//Table of contents
$( document ).ready(function() {
		var links = "<nav role='navigation' class='table-of-contents'>" +
			"<h3 class='h3'>Jump to Slide:</h3>" + 
			"<button class='close-menu'><img src='img/cross.svg'/></button>" +
			"<ul class='navlinks'>";

		var menuItem, el, title, link;

		$(".menu-entry").each(function() {
			el = $(this);
			title = el.attr("data-title");
			link = "#" + el.attr("id");

			menuItem = "<li><a href=" + link + ">" + title + "</a></li>";

			links += menuItem;
		});

		links += "</ul></nav>";

		$(".contents").prepend(links);
});

// Open/close ToC
$( document ).ready(function() {
	$(".no-menu .contents-menu").on('touchstart click', function(event) {
		$('body').removeClass("no-menu");
		$('body').addClass("menu-active");
		event.stopPropagation();
	});
	$(".menu-active .contents-menu").on('touchstart click', function(event) {
		$('body').removeClass("menu-active");
		$('body').addClass("menu-active");
		event.stopPropagation();
	});
	$(".close-menu").on('touchstart click', function(event) {
		$('body').removeClass("menu-active");
		event.stopPropagation();
	});
});

//Generate slide numbers 
$( document ).ready(function() {
	var i = 1;
	var el;

	$(".step").each(function(){
		el = $(this);

		el.attr('data-number', i);

		i++;

	});

	//Change slide number on transition

	 $(document).on('impress:stepenter', function(e) {
	 	var slidenumber = $(e.target).attr("data-number");

	 	$('.slide-number').text(slidenumber);
	 });
});

//Insert organization logos
$(document).ready(function() {

	//HTML for logo images
	var doh = '<img src="img/doh_logo.jpg" />';
	var poc = '<img src="img/POC_logo.png" class="poc_logo" />';

	//Check for data-org attribute on stepenter and create a variable if it exists
	$(document).on('impress:stepenter', function(event) {
		var org = $(event.target).attr("data-org");

		//Check value of variable and append the right logo or empty the element if no org
		if (org == 'doh') {
			$('.logo-wrap').html(doh);
		}

		if (org == 'poc') {
			$('.logo-wrap').html(poc);
		}

		if (!org) {
			$('.logo-wrap').empty();
		}
		
	});

});

//Toggles for teen birth maps
$( document ).ready(function() {
	$("#active-1999").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birth-1999.png");
	});

	$("#active-2007").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birth-2007.png");
	});

	$("#active-2012").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birth-2012.png");
	});

	$("#active-race1").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birthAmericanIndian.png");
	});

	$("#active-race2").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birthBlackorAfricanAmerican.png");
	});

	$("#active-race3").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birthHispanicorLatino.png");
	});

	$("#active-race4").click(function() {
		$(".controls li.highlighted").removeClass('highlighted');
		$(this).closest('li').addClass("highlighted");
		$('.map').attr('src', "R/teen-birthNon-HispanicWhite.png");
	});
});
