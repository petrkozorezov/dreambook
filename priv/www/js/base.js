getMainWindow = function() {
	$.ajax({
		url: "get_main_window",
		cache: false,
		beforeSend: function() {
			$('#main').html('Загружается');
		},
		success: function(html) {
			$("#main").html(html);
		}
	});
}
