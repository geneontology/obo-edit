<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>PhyloTree Loader</title>
</head>
<body>

<center><h1>PhyloTree Loader</h1></center>

	<table>
		
		<tr>
		
			<td>
				<input type="radio" name="formid" value="remoteload" group="g" onClick="remoteRadioClick();" /> Load Phylo Tree from FTP/HTTP Location
			</td>
			<td>
				<form action="/gold" id="remoteload">
					<input id="remote_text" type="text" name="filelocation" size="100" />
					<input type="hidden" name="servicename" value="phylo-tree-loader" />
				</form>
			</td>
		</tr>


		<tr>
		
			<td>
				<input type="radio" name="formid" value="upload" group="g" onClick="uploadRadioClick();" /> Upload GAF
			</td>
			<td>
				<form action="gold/" id="upload" method="post" enctype="multipart/form-data">
					<input type="text" name="servicename" value="phylo-tree-loader" style="display: none"/>
					<input type="file" name="uploadfile" id="uploadfield"/>

				</form>
			</td>
		</tr>



		
	</table>
	<br />
	<input type="submit" value="Submit GAF" onClick="submitForm()">
	

<script type="text/javascript" src="/scripts/jquery-1.5.1.min.js">
</script>

<script type="text/javascript">

$(document).ready(function() {
	$('#remote_text')[0].checked = true;
	remoteRadioClick();
	
 });


function remoteRadioClick(){
	$("#remote_text").removeAttr("disabled");
	$("#uploadfield").attr("disabled", "true");
}


function uploadRadioClick(){
	$("#remote_text").attr("disabled", "true");
	$("#uploadfield").removeAttr("disabled");
}

function submitForm(){
	var formid= $('input:radio[name="formid"]:checked').val();

	$("#"+formid).submit();
	
	return false;
}

</script>


</body>
</html>