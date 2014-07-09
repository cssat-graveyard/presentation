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
	$(".contents-menu").click(function(event) {
		event.stopPropagation();
		$('body').toggleClass("menu-active");
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