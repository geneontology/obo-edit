
function ExpandSection(idVariable, ImgName){

if (idVariable.style.display=="none")
	{
		idVariable.style.display="" ;
		if (ImgName.src != "") {ImgName.src = "dwnex.gif" ;}
	}
else{
	idVariable.style.display="none" ;
	if (ImgName.src != "") {ImgName.src = "rtex.gif" ;}
     }



}

function ContractSection(idVariable, ImgName){


if (idVariable.style.display==""){idVariable.style.display="none"}

  if (ImgName.src != "") {ImgName.src = "rtex.gif" ;}
}


