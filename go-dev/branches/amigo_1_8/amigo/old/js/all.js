if (document.getElementById && document.getElementsByTagName) {
	document.write('<style type="text/css"> <!-- .toggleable { display: none } --> <\/style>');
	var checkBoxes;
	var numberChecked;
	window.onload = initialize;
}

function initialize()
{	//initResultsTable();
	initFormHilite();
	startToggles();
}

function startToggles()
{	if (document.getElementById('results'))
	{	defToggle();
		listToggle();
		//synToggle();
	}
	
	if (document.getElementById('seqToggle'))
	{	seqToggle();
	}
	
	if (document.getElementById('xrefs'))
	{	xrefToggle();
	}
}

function defToggle()
{	if (document.createElement && document.createTextNode &&  document.getElementById('results'))
	{	var sr = document.getElementById('results');
		var defs = sr.getElementsByTagName('P');
			for (var i=0; i<defs.length;i++)
			{	if (defs[i].className == 'toggleable def' && defs[i].id)
				{	// var tog = document.createElement('SPAN');
					var togShow = document.createElement('A');
					togShow.id = defs[i].id + "toggle";
					togShow.href = '#' + defs[i].id;
					togShow.title = 'Show term definition';
					togShow.className = 'def-link';
					var togTxt = document.createTextNode('[show def]');
					togShow.appendChild(togTxt);

     var togHide = document.createElement('A');
     togHide.id = defs[i].id + "toggle-hide";
					togHide.href = '#' + defs[i].id;
					togHide.title = 'Hide term definition';
					togHide.className = 'def-link';
					var togTxt = document.createTextNode('[hide def]');
					togHide.appendChild(togTxt);
					togShow.onclick = function()
					{	switchVisUrl(this);
       changeToggleText(this, 'inline');
       return false;
     }
					togHide.onclick = function()
					{	switchVisUrl(this);
       changeToggleText(this, 'inline');
       return false;
     }

				//	tog.appendChild(togA);
					var par = defs[i].parentNode;
					var links = par.getElementsByTagName('A');
					if (links[1])
    { par.insertBefore(togHide,links[1]);
      par.insertBefore(togShow,links[1]);
      defs[i].style.display = 'none';
      togHide.style.display = 'none';
					}
					else
					{	links[0].parentNode.appendChild(togShow);
      links[0].parentNode.appendChild(togHide);
      defs[i].style.display = 'none';
      togHide.style.display = 'none';
    }
   }
  }
 }
}

function changeToggleText(url, display)
{	var id = url.href.match(/#(\w.+)/)[1];
	var showMe = document.getElementById(id + 'toggle');
	var hideMe = document.getElementById(id + 'toggle-hide');
	if (showMe && hideMe)
	{	if (!display)
		{	display = '';
		}
		if (showMe.style.display == 'none')
		{	showMe.style.display = display;
			hideMe.style.display = 'none';
		}
		else
		{	hideMe.style.display = display;
			showMe.style.display = 'none';
		}
	}
}

function showDef(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var def = document.getElementById(id);
	var toggler = document.getElementById(id + "toggle");
	if (def && toggler)
	{	def.style.display = '';
		toggler.style.display = 'none';
	}
}

function listToggle()
{	if (document.createElement && document.createTextNode)
	{	var sr = document.getElementById('results');
		// find any lists in the results
		if (sr)
		{	var uls = sr.getElementsByTagName('UL');
			for (var i=0; i<uls.length;i++)
			{	if (uls[i].id)
				{	var lis = uls[i].getElementsByTagName('LI');
					var lisl = lis.length - 2;
					var tog = document.createElement('LI');
					var togA = document.createElement('A');
					togA.href = '#' + uls[i].id;
					togA.title = 'View all synonyms';
					var togTxt = document.createTextNode('and ' + lisl + ' more');
					togA.onclick = function()
					{	showLis(this);
						return false;
					}
					togA.appendChild(togTxt);
					tog.appendChild(togA);
					for (var j=2; j<lis.length;j++)
					{	lis[j].style.display = 'none';
					}
					uls[i].insertBefore(tog,lis[2]);
				}
			}
		}
	}
}

function showLis(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var ul = document.getElementById(id);
	if (ul)
	{	var lis = ul.getElementsByTagName('LI');
		for (var j=0; j<lis.length;j++)
		{	lis[j].style.display = '';
		}
		lis[2].style.display = 'none';
	}
}

function seqToggle()
{	var seqlink = document.getElementById('seqToggle');
	var barseqlink = document.getElementById('barSeqToggle');

	if (seqlink && barseqlink)
	{	seqlink.onclick = function()
		{	switchVisUrl(this);
			return false;
		}
		barseqlink.onclick = function()
		{	var id = barseqlink.href.match(/#(\w.+)/)[1];
			if (id.style.display == 'none')
			{	switchVis(id);
			}
		}
		switchVisUrl(seqlink);
	}
}

function xrefToggle()
{	// go to section extRefs
	// each dt has a dd with a list in it
	// choose to show or hide the dd
	var dts = document.getElementById('xrefs').getElementsByTagName('DT');
	for (var i=0; i<dts.length; i++)
	{	var links = dts[i].getElementsByTagName('A');
		for (var a=0; a<links.length; a++)
		{	links[a].onclick = function()
			{	switchImg(this);
				return false;
			}
			preSwitchImg(links[a]);
		}
	}
}

function switchImg(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var idDiv = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	if (idDiv && i)
	{	if (idDiv.style.display == 'none')
		{	idDiv.style.display = '';
			i.src = "../images/open.gif"
			i.title = "Hide cross references";
		}
		else
		{	idDiv.style.display = 'none';
			i.src = "../images/openable.gif";
			i.title = "Show cross references";
		}
	}
}

function preSwitchImg(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var idDiv = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	var lis = idDiv.getElementsByTagName('LI');
	if (lis.length > 2)
	{	if (idDiv && i)
		{	if (idDiv.style.display == 'none')
			{	idDiv.style.display = '';
				i.src = "../images/open.gif"
				i.title = "Hide cross references";
			}
			else
			{	idDiv.style.display = 'none';
				i.src = "../images/openable.gif";
				i.title = "Show cross references";
			}
		}
	}
}

function synToggle()
{	if (document.createElement && document.createTextNode)
	{	var sr = document.getElementById('results');
		// find the synonym list
		// synonym lists with an ID are those which have more than one syn
		if (sr)
		{	var uls = sr.getElementsByTagName('UL');
			for (var i=1; i<uls.length;i++)
			{	if (uls[i].id)
				{	var lis = uls[i].getElementsByTagName('LI');
					var lisl = lis.length - 2;
					var tog = document.createElement('LI');
					var togA = document.createElement('A');
					togA.href = '#' + uls[i].id;
					togA.title = 'View all synonyms';
					var togTxt = document.createTextNode('and ' + lisl + ' more');
					togA.onclick = function()
					{	showLis(this);
						return false;
					}
					togA.appendChild(togTxt);
					tog.appendChild(togA);
					for (var j=2; j<lis.length;j++)
					{	lis[j].style.display = 'none';
					}
					uls[i].insertBefore(tog,lis[2]);
				}
			}
		}
	}
}




/* get an object from an URL */
function switchVisUrl(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	switchVis(id);
}

/* toggle an element's display */
function switchVis(obj) {
	var el = document.getElementById(obj);
	if ( el.style.display != 'none' ) {
		el.style.display = 'none';
	}
	else {
		el.style.display = '';
	}
}


function initFormHilite()
{	if (document.getElementById('frontForm'))
	{	document.frontForm.query.focus();
		for (var i=0; i < document.frontForm.search_constraint.length; i++)
		{	document.frontForm.search_constraint[i].onclick = function()
			{	hilite();
			}
		}
		hilite();
	}
	else return false;
}

function hilite()
{	// find all the inputs with name "search_constraint"
	for (var i=0; i < document.frontForm.search_constraint.length; i++)
	{	if (document.frontForm.search_constraint[i].checked)
		{	document.frontForm.search_constraint[i].parentNode.className = "formHilite";
		}
		else
		{	document.frontForm.search_constraint[i].parentNode.className = "";
		}
	}
	return true;
}

function initResultsTable()
{	var tables = document.getElementsByTagName('TABLE');
	var results = document.getElementById("results");
	if (results && tables)
  { checkBoxes = document.getElementsByName("item");
		for (var i = 0; i < checkBoxes.length; i++)
		{	checkBoxes[i].onclick = DataTableCheckBox_Click;
		}

		if (document.createElement)
    { AddToggles('options', '');
//      AddToggles('options', '-top');
  //    AddToggles('options', '-bottom');
		}
		var suffix = '';
 document.getElementById('checkall' + suffix).onclick = SelectAllRows_Click(suffix);
 document.getElementById('clearall' + suffix).onclick = SelectAllRows_Click(suffix);
 document.getElementById('selectallrows' + suffix).onclick = SelectAllRows_Click(suffix);
		numberChecked = 0;
	}
	else return false;
}

function AddToggles(parent, suffix)
{ if (document.getElementById(parent + suffix))
 {
var all = document.createElement("INPUT");
all.setAttribute("type", "button");
all.setAttribute("value", "Check all");
	all.title = "Check all";
	all.id = 'checkall' + suffix;
// var all = document.createElement("SPAN");
// var alltxt = document.createTextNode(' all ');
// all.appendChild(alltxt);
all.onclick = function() { SelectAllRows_Click(suffix) }

var none = document.createElement("INPUT");
none.setAttribute("type", "button");
none.setAttribute("value", "Clear all");
	none.title = "Clear all";
	none.id = 'clearall' + suffix;
// var none = document.createElement("SPAN");
// var nonetxt = document.createTextNode(' none ');
// none.appendChild(nonetxt);
none.onclick = function() { SelectAllRows_Click(suffix) }

	var cbox = document.createElement("INPUT");
	cbox.title = "Select or deselect all";
	cbox.setAttribute("type", "checkbox");
	cbox.id = 'selectallrows' + suffix;
cbox.onclick = function() { SelectAllRows_Click(suffix) }

	var select = document.createTextNode('Select ');

 var td = document.getElementById(parent + suffix);
 var tr = td.parentNode;
 var tds = tr.getElementsByTagName('TD');
		
	tds[0].insertBefore(cbox, tds[0].firstChild);
	tds[1].insertBefore(none, tds[1].firstChild);
	tds[1].insertBefore(all, tds[1].firstChild);
	tds[1].insertBefore(select, tds[1].firstChild);

 document.getElementById('checkall' + suffix).onclick = SelectAllRows_Click(suffix);
 document.getElementById('clearall' + suffix).onclick = SelectAllRows_Click(suffix);
 document.getElementById('selectallrows' + suffix).onclick = SelectAllRows_Click(suffix);

//var ids = [cbox, none, all];
// YAHOO.util.Event.addListener(ids, "click", SelectAllRows_Click);

//	document.getElementById('checkall' + suffix).className = 'selector';
//	document.getElementById('clearall' + suffix).className = 'selector';
 }
}

var Clicked = function(e) {
	alert("I was clicked!");
}

function SelectAllRows_Click(suffix)
{ //var checkBoxes = document.getElementsByName("gp");
  console.log("called SelectAllRows_Click");
  var status;
  var selectall = 'selectallrows' + suffix;
  var checkall = 'checkall' + suffix;
  var clearall = 'clearall' + suffix;
  if (document.getElementById('selectallrows' + suffix))
  { status = document.getElementById('selectallrows' + suffix).checked;
  }
  else
  { return false;
  }
 if(this.id == 'clearall' + suffix || this.id == 'checkall' + suffix)
 { status = (this.id == 'clearall' + suffix) ? false : true;
 }

 if (status)
 { for (var i = 0; i < checkBoxes.length; i++)
  { var oTR = checkBoxes[i].parentNode.parentNode;
   checkBoxes[i].checked = status;
   if (oTR.className)
   { if (oTR.className.indexOf('selected') == -1) oTR.className += ' selected';
   }
   else oTR.className = 'selected';
  }
 }
 else
 { for (var i = 0; i < checkBoxes.length; i++)
  { checkBoxes[i].checked = status;
   if (checkBoxes[i].parentNode.parentNode.className == 'selected')
   { checkBoxes[i].parentNode.parentNode.className = '';
   }
   else
   { checkBoxes[i].parentNode.parentNode.className = checkBoxes[i].parentNode.parentNode.className.replace(/ selected/g, "")
   }
  }
 }
 document.getElementById('selectallrows' + suffix).checked = status;
	numberChecked = (status) ? checkBoxes.length : 0;
}

function DataTableCheckBox_Click()
{	var oTR = this.parentNode.parentNode;
	if(this.checked) 
	{	if(oTR.className) oTR.className += ' selected';
		else oTR.className = 'selected';
		numberChecked++;
	}
	else 
	{	if (oTR.className == 'selected') oTR.className = '';
		else
		{	oTR.className = oTR.className.replace(/ selected/g, "");
		}
		numberChecked--;
	}
	if (document.getElementById('selectallrows'))
	{	document.getElementById('selectallrows').checked = (numberChecked == checkBoxes.length) ? true : false;
	}
}
