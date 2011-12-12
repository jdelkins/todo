var md5 = "";
var default_listing = "";
var sort = "number";
var cmdHistory = [""];
var stashHistory = "";
var curHistInd = 1;

function jsonml_bind(elem) {
    Element.extend(elem);
    // fix an HTML quirk of JsonML.cs: it sets colSpan instead of colspan, and uses string type
    // instead of integer type
    if ((document.contentType === "application/xhtml+xml" ||
	 document.documentElement.tagName === "html") && // lowercase html apparently indicates xhtml
	elem.tagName.toLowerCase() === "td" &&
	elem.readAttribute('colSpan'))
    {
	var cs = Number(elem.readAttribute('colSpan'));
	elem.removeAttribute('colSpan');
	elem.setAttribute('colspan', cs);
    }
    if (elem.tagName.toLowerCase() === "input" && elem.hasClassName('taskcheckbox')) {
	var taskid = elem.readAttribute('taskid');
	if (elem.hasClassName('dead')) {
	    elem.setAttribute('checked', true);
	    // Work around for a notorious and ridiculous IE DOM bug -- can't change
	    // the checked attribute until the input element is "live". Set a timer, what else?
	    new PeriodicalExecuter(function(pe) {
		elem.setAttribute('checked', true);
		pe.stop();
	    }, 0.1);
	    elem.onclick = function () { perform_command("sub " + taskid + " '[xX]:[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]' ''"); };
	} else
	    elem.onclick = function () { perform_command("do " + taskid); };
    }
    if (elem.tagName.toLowerCase() === "img" && elem.hasClassName('taskediticon')) {
	var taskid = elem.readAttribute('taskid');
	elem.onclick = function() {
	    perform_command("edit " + taskid);
	};
    }
    if ((elem.tagName.toLowerCase() === "span") && elem.hasClassName('taskhead')) {
	var st = elem.readAttribute("id");
	if (st === sort)
	    elem.addClassName('sorted');
	elem.onclick = function() {
	    $$('span.taskhead').invoke('removeClassName', 'sorted');
	    elem.addClassName('sorted');
	    sort = st;
	    perform_command("");
	    return false;
	};
    }
    if ((elem.tagName.toLowerCase() === "button") && (elem.readAttribute('id') === 'edittask_apply')) {
	elem.onclick = function() {
	    var txt = $('edittask').value
	    var num = $('edittask_number').firstChild.nodeValue;
	    perform_command("replace " + num + " " + txt);
	};
    }
    if ((elem.tagName.toLowerCase() === "button") && (elem.readAttribute('id') === 'edittask_cancel')) {
	elem.onclick = function() {
	    perform_command("");
	};
    }
    return elem;
}

function process_results(transport) {
    var obj = transport.responseJSON;
    var theError = null;
    if (!obj)
	alert('responseJSON is null');
    if (obj['sort'])
	sort = obj['sort'];
    if (obj['error'])
	theError = obj['error'].parseJsonML();
    var theTable = obj['listing'].parseJsonML(jsonml_bind);
    var resultsDiv = $('results');
    while (resultsDiv.lastChild)
	resultsDiv.removeChild(resultsDiv.lastChild);
    if (theError)
	resultsDiv.appendChild(theError);
    resultsDiv.appendChild(theTable);
    md5 = obj['md5'];
    default_listing = obj['default_listing']
    while ($('filtered_by').lastChild)
	$('filtered_by').removeChild($('filtered_by').lastChild);
    if (obj['filter'] && obj['filter'].length > 0) {
	var tgt = $('filtered_by');
	var sp = new Element('span', { 'class': 'filterlabel' }).update("Filtered by '".escapeHTML());
	obj['filter'].each(
	    function(f,i) {
		var ft = new Element('span', { 'class': 'filtertext' }).update(f.escapeHTML());
		ft.onclick = function() {
		    perform_command("", f);
		};
		sp.appendChild(ft);
		if (obj['filter'].length - 1 > i)
		    sp.appendChild(document.createTextNode("' and '".escapeHTML()));
	    }
	);
	sp.appendChild(document.createTextNode("'".escapeHTML()));
	tgt.appendChild(sp);
    }
    if ($('edittask'))
	$('edittask').focus();
    else
	$('command').focus();
}

function process_error(transport) {
    alert('CGI returned an error: ' + transport.status)
    $('results').innerHTML = transport.responseText;
}

function perform_command(cmd, kill_filter) {
    var params = new Hash({
	'md5': md5,
	'sort': sort,
	'default_listing': default_listing,
	'command': cmd,
	'kill_filter': kill_filter
    });
    new Ajax.Request('todo.cgi', {
	method:'post',
	parameters: params,
	onSuccess: process_results,
	onFailure: process_error,
	onException: function(rq, ex) { alert('Exception: ' + ex.toString()) }
    });
}

function command_key(ev) {
    switch (ev.keyCode) {
    case 13: // return/enter
	var t = $('command').value;
	if ($('welcome')) $('welcome').remove();
	perform_command(t);
	cmdHistory.push(t);
	curHistInd = cmdHistory.length;
	stashHistory = "";
	$('command').value = "";
	break;
    case 33: // page up
	window.scrollBy(0,-200);
	break;
    case 34: // page down
	window.scrollBy(0,200);
	break;
    case 38: // up arrow
	if (curHistInd == cmdHistory.length)
	    stashHistory = $('command').value;
	if (--curHistInd < 0) curHistInd = 0;
	$('command').value = cmdHistory[curHistInd];
	break;
    case 40: // down arrow
	if (curHistInd >= cmdHistory.length)
	    curHistInd = cmdHistory.length;
	else if (curHistInd == cmdHistory.length - 1) {
	    $('command').value = stashHistory;
	    curHistInd++;
	} else
	    $('command').value = cmdHistory[++curHistInd];
	break;
    }
}
