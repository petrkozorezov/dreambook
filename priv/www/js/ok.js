

SocialInit = function(api_server, apiconnection) {
	//Initiatlize API
	console.log("API initialization: " , api_server, ", ", apiconnection);
	FAPI.init(api_server, apiconnection,
	function() {
		console.log("API initialization ok");
	}, function(error){
		console.log("API initialization failed: ", error);
	});

//	var  options = "[{\"name\":\"" + name1 + "\", \"price\":\"2\", \"code\":\"qCoins1\"},{\"name\":\"" + name1 + "\", \"price\":\"3\", \"code\":\"qCoins2\"}]";
	var  permissions = "[\"PUBLISH TO STREAM\"]";

	//Callback function 
	function API_callback(method, result, data){
	    if(method == 'showPayment' && status == 'ok') {
	    	window.location.reload();
	    }
	}
}

SocialInvite = function() {
	console.log("invite");
	FAPI.UI.showInvite('Default text', 'customAttr=customValue');
}


SocialPay = function(text, val, cb) {
	console.log("pay");
	FAPI.UI.showPayment('Product name', text, val);
	if(cb) {
		cb();
	}
}
