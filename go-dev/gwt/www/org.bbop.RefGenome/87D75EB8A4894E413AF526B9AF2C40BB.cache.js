(function(){var $wnd = window;var $doc = $wnd.document;var $moduleName, $moduleBase;var _,ts='com.google.gwt.core.client.',us='com.google.gwt.lang.',vs='com.google.gwt.user.client.',ws='com.google.gwt.user.client.impl.',xs='com.google.gwt.user.client.ui.',ys='com.google.gwt.user.client.ui.impl.',zs='java.lang.',As='java.util.',Bs='org.bbop.client.',Cs='org.bbop.client.Pages.RefGenome.';function js(){}
function Am(a){return this===a;}
function Bm(){return qn(this);}
function ym(){}
_=ym.prototype={};_.eQ=Am;_.hC=Bm;_.tI=1;var o=null;function r(a){return a==null?0:a.$H?a.$H:(a.$H=t());}
function s(a){return a==null?0:a.$H?a.$H:(a.$H=t());}
function t(){return ++u;}
var u=0;function x(b,a){if(!mb(a,2)){return false;}return B(b,lb(a,2));}
function y(a){return r(a);}
function z(){return [];}
function A(){return {};}
function C(a){return x(this,a);}
function B(a,b){return a===b;}
function D(){return y(this);}
function v(){}
_=v.prototype=new ym();_.eQ=C;_.hC=D;_.tI=7;function F(c,a,d,b,e){c.a=a;c.b=b;e;c.tI=d;return c;}
function bb(a,b,c){return a[b]=c;}
function cb(b,a){return b[a];}
function db(a){return a.length;}
function fb(e,d,c,b,a){return eb(e,d,c,b,0,db(b),a);}
function eb(j,i,g,c,e,a,b){var d,f,h;if((f=cb(c,e))<0){throw new wm();}h=F(new E(),f,cb(i,e),cb(g,e),j);++e;if(e<a){j=gn(j,1);for(d=0;d<f;++d){bb(h,d,eb(j,i,g,c,e,a,b));}}else{for(d=0;d<f;++d){bb(h,d,b);}}return h;}
function gb(a,b,c){if(c!==null&&a.b!=0&& !mb(c,a.b)){throw new dm();}return bb(a,b,c);}
function E(){}
_=E.prototype=new ym();_.tI=0;function jb(b,a){return !(!(b&&qb[b][a]));}
function kb(a){return String.fromCharCode(a);}
function lb(b,a){if(b!=null)jb(b.tI,a)||pb();return b;}
function mb(b,a){return b!=null&&jb(b.tI,a);}
function nb(a){return a&65535;}
function pb(){throw new gm();}
function ob(a){if(a!==null){throw new gm();}return a;}
function rb(b,d){_=d.prototype;if(b&& !(b.tI>=_.tI)){var c=b.toString;for(var a in _){b[a]=_[a];}b.toString=c;}return b;}
var qb;function vb(){vb=js;vc=Cp(new Ap());{pc=new Dd();fe(pc);}}
function wb(b,a){vb();me(pc,b,a);}
function xb(a,b){vb();return Fd(pc,a,b);}
function yb(){vb();return oe(pc,'div');}
function zb(){vb();return pe(pc,'password');}
function Ab(a){vb();return ae(pc,a);}
function Bb(){vb();return pe(pc,'text');}
function Cb(){vb();return oe(pc,'label');}
function Db(){vb();return oe(pc,'span');}
function Eb(){vb();return oe(pc,'tbody');}
function Fb(){vb();return oe(pc,'td');}
function ac(){vb();return oe(pc,'tr');}
function bc(){vb();return oe(pc,'table');}
function ec(b,a,d){vb();var c;c=o;{dc(b,a,d);}}
function dc(b,a,c){vb();var d;if(a===uc){if(jc(b)==8192){uc=null;}}d=cc;cc=b;try{c.E(b);}finally{cc=d;}}
function fc(b,a){vb();qe(pc,b,a);}
function gc(a){vb();return re(pc,a);}
function hc(a){vb();return be(pc,a);}
function ic(a){vb();return ce(pc,a);}
function jc(a){vb();return se(pc,a);}
function kc(a){vb();de(pc,a);}
function mc(a,b){vb();return ue(pc,a,b);}
function lc(a,b){vb();return te(pc,a,b);}
function nc(a){vb();return ve(pc,a);}
function oc(a){vb();return ee(pc,a);}
function qc(b,a){vb();return ge(pc,b,a);}
function rc(a){vb();var b,c;c=true;if(vc.b>0){b=ob(aq(vc,vc.b-1));if(!(c=null.mb())){fc(a,true);kc(a);}}return c;}
function sc(a){vb();if(uc!==null&&xb(a,uc)){uc=null;}he(pc,a);}
function tc(b,a){vb();we(pc,b,a);}
function wc(a){vb();uc=a;ie(pc,a);}
function yc(a,b,c){vb();ye(pc,a,b,c);}
function xc(a,b,c){vb();xe(pc,a,b,c);}
function zc(a,b){vb();ze(pc,a,b);}
function Ac(a,b){vb();Ae(pc,a,b);}
function Bc(a,b){vb();je(pc,a,b);}
function Cc(b,a,c){vb();Be(pc,b,a,c);}
function Dc(a,b){vb();ke(pc,a,b);}
var cc=null,pc=null,uc=null,vc;function ad(a){if(mb(a,4)){return xb(this,lb(a,4));}return x(rb(this,Ec),a);}
function bd(){return y(rb(this,Ec));}
function Ec(){}
_=Ec.prototype=new v();_.eQ=ad;_.hC=bd;_.tI=8;function fd(a){return x(rb(this,cd),a);}
function gd(){return y(rb(this,cd));}
function cd(){}
_=cd.prototype=new v();_.eQ=fd;_.hC=gd;_.tI=9;function md(){md=js;od=Cp(new Ap());{nd();}}
function nd(){md();sd(new id());}
var od;function kd(){while((md(),od).b>0){ob(aq((md(),od),0)).mb();}}
function ld(){return null;}
function id(){}
_=id.prototype=new ym();_.fb=kd;_.gb=ld;_.tI=10;function rd(){rd=js;td=Cp(new Ap());Bd=Cp(new Ap());{xd();}}
function sd(a){rd();Dp(td,a);}
function ud(){rd();var a,b;for(a=io(td);ao(a);){b=lb(bo(a),5);b.fb();}}
function vd(){rd();var a,b,c,d;d=null;for(a=io(td);ao(a);){b=lb(bo(a),5);c=b.gb();{d=c;}}return d;}
function wd(){rd();var a,b;for(a=io(Bd);ao(a);){b=ob(bo(a));null.mb();}}
function xd(){rd();__gwt_initHandlers(function(){Ad();},function(){return zd();},function(){yd();$wnd.onresize=null;$wnd.onbeforeclose=null;$wnd.onclose=null;});}
function yd(){rd();var a;a=o;{ud();}}
function zd(){rd();var a;a=o;{return vd();}}
function Ad(){rd();var a;a=o;{wd();}}
var td,Bd;function me(c,b,a){b.appendChild(a);}
function oe(b,a){return $doc.createElement(a);}
function pe(b,c){var a=$doc.createElement('INPUT');a.type=c;return a;}
function qe(c,b,a){b.cancelBubble=a;}
function re(b,a){return a.which||(a.keyCode|| -1);}
function se(b,a){switch(a.type){case 'blur':return 4096;case 'change':return 1024;case 'click':return 1;case 'dblclick':return 2;case 'focus':return 2048;case 'keydown':return 128;case 'keypress':return 256;case 'keyup':return 512;case 'load':return 32768;case 'losecapture':return 8192;case 'mousedown':return 4;case 'mousemove':return 64;case 'mouseout':return 32;case 'mouseover':return 16;case 'mouseup':return 8;case 'scroll':return 16384;case 'error':return 65536;case 'mousewheel':return 131072;case 'DOMMouseScroll':return 131072;}}
function ue(d,a,b){var c=a[b];return c==null?null:String(c);}
function te(c,a,b){return !(!a[b]);}
function ve(b,a){return a.__eventBits||0;}
function we(c,b,a){b.removeChild(a);}
function ye(c,a,b,d){a[b]=d;}
function xe(c,a,b,d){a[b]=d;}
function ze(c,a,b){a.__listener=b;}
function Ae(c,a,b){if(!b){b='';}a.innerHTML=b;}
function Be(c,b,a,d){b.style[a]=d;}
function Cd(){}
_=Cd.prototype=new ym();_.tI=0;function Fd(c,a,b){if(!a&& !b)return true;else if(!a|| !b)return false;return a.uniqueID==b.uniqueID;}
function ae(b,a){return $doc.createElement("<INPUT type='RADIO' name='"+a+"'>");}
function be(b,a){return a.srcElement||null;}
function ce(b,a){return a.toElement||null;}
function de(b,a){a.returnValue=false;}
function ee(c,a){var b=a.parentElement;return b||null;}
function fe(d){try{$doc.execCommand('BackgroundImageCache',false,true);}catch(a){}$wnd.__dispatchEvent=function(){var c=le;le=this;if($wnd.event.returnValue==null){$wnd.event.returnValue=true;if(!rc($wnd.event)){le=c;return;}}var b,a=this;while(a&& !(b=a.__listener))a=a.parentElement;if(b)ec($wnd.event,a,b);le=c;};$wnd.__dispatchDblClickEvent=function(){var a=$doc.createEventObject();this.fireEvent('onclick',a);if(this.__eventBits&2)$wnd.__dispatchEvent.call(this);};$doc.body.onclick=$doc.body.onmousedown=$doc.body.onmouseup=$doc.body.onmousemove=$doc.body.onmousewheel=$doc.body.onkeydown=$doc.body.onkeypress=$doc.body.onkeyup=$doc.body.onfocus=$doc.body.onblur=$doc.body.ondblclick=$wnd.__dispatchEvent;}
function ge(c,b,a){while(a){if(b.uniqueID==a.uniqueID)return true;a=a.parentElement;}return false;}
function he(b,a){a.releaseCapture();}
function ie(b,a){a.setCapture();}
function je(c,a,b){if(!b)b='';a.innerText=b;}
function ke(c,b,a){b.__eventBits=a;b.onclick=a&1?$wnd.__dispatchEvent:null;b.ondblclick=a&(1|2)?$wnd.__dispatchDblClickEvent:null;b.onmousedown=a&4?$wnd.__dispatchEvent:null;b.onmouseup=a&8?$wnd.__dispatchEvent:null;b.onmouseover=a&16?$wnd.__dispatchEvent:null;b.onmouseout=a&32?$wnd.__dispatchEvent:null;b.onmousemove=a&64?$wnd.__dispatchEvent:null;b.onkeydown=a&128?$wnd.__dispatchEvent:null;b.onkeypress=a&256?$wnd.__dispatchEvent:null;b.onkeyup=a&512?$wnd.__dispatchEvent:null;b.onchange=a&1024?$wnd.__dispatchEvent:null;b.onfocus=a&2048?$wnd.__dispatchEvent:null;b.onblur=a&4096?$wnd.__dispatchEvent:null;b.onlosecapture=a&8192?$wnd.__dispatchEvent:null;b.onscroll=a&16384?$wnd.__dispatchEvent:null;b.onload=a&32768?$wnd.__dispatchEvent:null;b.onerror=a&65536?$wnd.__dispatchEvent:null;b.onmousewheel=a&131072?$wnd.__dispatchEvent:null;}
function Dd(){}
_=Dd.prototype=new Cd();_.tI=0;var le=null;function dk(b,a){ek(b,gk(b)+kb(45)+a);}
function ek(b,a){qk(b.m,a,true);}
function gk(a){return ok(a.m);}
function hk(b,a){ik(b,gk(b)+kb(45)+a);}
function ik(b,a){qk(b.m,a,false);}
function jk(d,b,a){var c=b.parentNode;if(!c){return;}c.insertBefore(a,b);c.removeChild(b);}
function kk(b,a){if(b.m!==null){jk(b,b.m,a);}b.m=a;}
function lk(b,a){pk(b.m,a);}
function mk(b,a){Dc(b.m,a|nc(b.m));}
function nk(a){return mc(a,'className');}
function ok(a){var b,c;b=nk(a);c=cn(b,32);if(c>=0){return hn(b,0,c);}return b;}
function pk(a,b){yc(a,'className',b);}
function qk(c,j,a){var b,d,e,f,g,h,i;if(c===null){throw Dm(new Cm(),'Null widget handle. If you are creating a composite, ensure that initWidget() has been called.');}j=jn(j);if(fn(j)==0){throw om(new nm(),'Style names cannot be empty');}i=nk(c);e=dn(i,j);while(e!=(-1)){if(e==0||an(i,e-1)==32){f=e+fn(j);g=fn(i);if(f==g||f<g&&an(i,f)==32){break;}}e=en(i,j,e+1);}if(a){if(e==(-1)){if(fn(i)>0){i+=' ';}yc(c,'className',i+j);}}else{if(e!=(-1)){b=jn(hn(i,0,e));d=jn(gn(i,e+fn(j)));if(fn(b)==0){h=d;}else if(fn(d)==0){h=b;}else{h=b+' '+d;}yc(c,'className',h);}}}
function ck(){}
_=ck.prototype=new ym();_.tI=0;_.m=null;function kl(a){if(a.k){throw rm(new qm(),"Should only call onAttach when the widget is detached from the browser's document");}a.k=true;zc(a.m,a);a.r();a.db();}
function ll(a){if(!a.k){throw rm(new qm(),"Should only call onDetach when the widget is attached to the browser's document");}try{a.eb();}finally{a.s();zc(a.m,null);a.k=false;}}
function ml(a){if(a.l!==null){a.l.ib(a);}else if(a.l!==null){throw rm(new qm(),"This widget's parent does not implement HasWidgets");}}
function nl(b,a){if(b.k){zc(b.m,null);}kk(b,a);if(b.k){zc(a,b);}}
function ol(c,b){var a;a=c.l;if(b===null){if(a!==null&&a.k){c.cb();}c.l=null;}else{if(a!==null){throw rm(new qm(),'Cannot set a new parent without first clearing the old parent');}c.l=b;if(b.k){c.D();}}}
function pl(){}
function ql(){}
function rl(){kl(this);}
function sl(a){}
function tl(){ll(this);}
function ul(){}
function vl(){}
function wl(a){nl(this,a);}
function yk(){}
_=yk.prototype=new ck();_.r=pl;_.s=ql;_.D=rl;_.E=sl;_.cb=tl;_.db=ul;_.eb=vl;_.jb=wl;_.tI=11;_.k=false;_.l=null;function si(b,a){ol(a,b);}
function ui(b,a){ol(a,null);}
function vi(){var a,b;for(b=this.A();Dk(b);){a=Ek(b);a.D();}}
function wi(){var a,b;for(b=this.A();Dk(b);){a=Ek(b);a.cb();}}
function xi(){}
function yi(){}
function ri(){}
_=ri.prototype=new yk();_.r=vi;_.s=wi;_.db=xi;_.eb=yi;_.tI=12;function wf(a){a.e=bl(new zk(),a);}
function xf(a){wf(a);return a;}
function yf(c,a,b){ml(a);cl(c.e,a);wb(b,a.m);si(c,a);}
function Af(b,c){var a;if(c.l!==b){return false;}ui(b,c);a=c.m;tc(oc(a),a);il(b.e,c);return true;}
function Bf(){return gl(this.e);}
function Cf(a){return Af(this,a);}
function vf(){}
_=vf.prototype=new ri();_.A=Bf;_.ib=Cf;_.tI=13;function De(a){xf(a);a.jb(yb());Cc(a.m,'position','relative');Cc(a.m,'overflow','hidden');return a;}
function Ee(a,b){yf(a,b,a.m);}
function af(a){Cc(a,'left','');Cc(a,'top','');Cc(a,'position','');}
function bf(b){var a;a=Af(this,b);if(a){af(b.m);}return a;}
function Ce(){}
_=Ce.prototype=new vf();_.ib=bf;_.tI=14;function qh(){qh=js;El(),bm;}
function ph(b,a){El(),bm;sh(b,a);return b;}
function rh(b,a){switch(jc(a)){case 1:break;case 4096:case 2048:break;case 128:case 512:case 256:break;}}
function sh(b,a){nl(b,a);mk(b,7041);}
function th(){return !lc(this.m,'disabled');}
function uh(a){rh(this,a);}
function vh(a){sh(this,a);}
function oh(){}
_=oh.prototype=new yk();_.z=th;_.E=uh;_.jb=vh;_.tI=15;function ef(){ef=js;El(),bm;}
function df(b,a){El(),bm;ph(b,a);return b;}
function cf(){}
_=cf.prototype=new oh();_.tI=16;function gf(a){xf(a);a.d=bc();a.c=Eb();wb(a.d,a.c);a.jb(a.d);return a;}
function jf(c,b,a){yc(b,'align',a.a);}
function kf(c,b,a){Cc(b,'verticalAlign',a.a);}
function ff(){}
_=ff.prototype=new vf();_.tI=17;_.c=null;_.d=null;function nf(){nf=js;El(),bm;}
function mf(b,a){var c;El(),bm;df(b,Db());b.a=a;b.b=Cb();Dc(b.a,nc(b.m));Dc(b.m,0);wb(b.m,b.a);wb(b.m,b.b);c='check'+ ++uf;yc(b.a,'id',c);yc(b.b,'htmlFor',c);return b;}
function of(b){var a;a=b.k?'checked':'defaultChecked';return lc(b.a,a);}
function pf(b,a){xc(b.a,'checked',a);xc(b.a,'defaultChecked',a);}
function qf(b,a){Bc(b.b,a);}
function rf(){return !lc(this.a,'disabled');}
function sf(){zc(this.a,this);}
function tf(){zc(this.a,null);pf(this,of(this));}
function lf(){}
_=lf.prototype=new cf();_.z=rf;_.db=sf;_.eb=tf;_.tI=18;_.a=null;_.b=null;var uf=0;function lg(){lg=js;El(),bm;}
function ig(a,b){El(),bm;hg(a);fg(a.h,b);return a;}
function jg(b,c,a){El(),bm;ig(b,c);fg(qg(b),a);return b;}
function hg(a){El(),bm;df(a,Fl((mh(),nh)));mk(a,6269);bh(a,mg(a,null,'up',0));lk(a,'gwt-CustomButton');return a;}
function kg(a){if(a.f||a.g){sc(a.m);a.f=false;a.g=false;a.F();}}
function mg(d,a,c,b){return Ff(new Ef(),a,d,c,b);}
function ng(a){if(a.a===null){zg(a,a.h);}}
function og(a){ng(a);return a.a;}
function pg(a){if(a.d===null){Ag(a,mg(a,qg(a),'down-disabled',5));}return a.d;}
function qg(a){if(a.c===null){Bg(a,mg(a,a.h,'down',1));}return a.c;}
function rg(a){if(a.e===null){Cg(a,mg(a,qg(a),'down-hovering',3));}return a.e;}
function sg(b,a){switch(a){case 1:return qg(b);case 0:return b.h;case 3:return rg(b);case 2:return ug(b);case 4:return tg(b);case 5:return pg(b);default:throw rm(new qm(),a+' is not a known face id.');}}
function tg(a){if(a.i===null){ah(a,mg(a,a.h,'up-disabled',4));}return a.i;}
function ug(a){if(a.j===null){ch(a,mg(a,a.h,'up-hovering',2));}return a.j;}
function vg(a){return (1&og(a).a)>0;}
function wg(a){return (2&og(a).a)>0;}
function zg(b,a){if(b.a!==a){if(b.a!==null){hk(b,b.a.b);}b.a=a;xg(b,eg(a));dk(b,b.a.b);}}
function yg(c,a){var b;b=sg(c,a);zg(c,b);}
function xg(b,a){if(b.b!==a){if(b.b!==null){tc(b.m,b.b);}b.b=a;wb(b.m,b.b);}}
function Dg(b,a){if(a!=vg(b)){dh(b);}}
function Ag(b,a){b.d=a;}
function Bg(b,a){b.c=a;}
function Cg(b,a){b.e=a;}
function Eg(b,a){if(a){Bl((mh(),nh),b.m);}else{Dl((mh(),nh),b.m);}}
function Fg(b,a){if(a!=wg(b)){eh(b);}}
function ah(a,b){a.i=b;}
function bh(a,b){a.h=b;}
function ch(a,b){a.j=b;}
function dh(b){var a;a=og(b).a^1;yg(b,a);}
function eh(b){var a;a=og(b).a^2;a&=(-5);yg(b,a);}
function fh(){ng(this);kl(this);}
function gh(a){var b,c;if(this.z()==false){return;}c=jc(a);switch(c){case 4:Eg(this,true);this.ab();wc(this.m);this.f=true;kc(a);break;case 8:if(this.f){this.f=false;sc(this.m);if(wg(this)){this.bb();}}break;case 64:if(this.f){kc(a);}break;case 32:if(qc(this.m,hc(a))&& !qc(this.m,ic(a))){if(this.f){this.F();}Fg(this,false);}break;case 16:if(qc(this.m,hc(a))){Fg(this,true);if(this.f){this.ab();}}break;case 1:return;case 4096:if(this.g){this.g=false;this.F();}break;case 8192:if(this.f){this.f=false;this.F();}break;}rh(this,a);b=nb(gc(a));switch(c){case 128:if(b==32){this.g=true;this.ab();}break;case 512:if(this.g&&b==32){this.g=false;this.bb();}break;case 256:if(b==10||b==13){this.ab();this.bb();}break;}}
function jh(){}
function hh(){}
function ih(){}
function kh(){ll(this);kg(this);}
function Df(){}
_=Df.prototype=new cf();_.D=fh;_.E=gh;_.bb=jh;_.F=hh;_.ab=ih;_.cb=kh;_.tI=19;_.a=null;_.b=null;_.c=null;_.d=null;_.e=null;_.f=false;_.g=false;_.h=null;_.i=null;_.j=null;function cg(c,a,b){c.e=b;c.c=a;return c;}
function eg(a){if(a.d===null){if(a.c===null){a.d=yb();return a.d;}else{return eg(a.c);}}else{return a.d;}}
function fg(b,a){b.d=yb();qk(b.d,'html-face',true);Bc(b.d,a);gg(b);}
function gg(a){if(a.e.a!==null&&eg(a.e.a)===eg(a)){xg(a.e,a.d);}}
function bg(){}
_=bg.prototype=new ym();_.tI=0;_.c=null;_.d=null;function Ff(c,a,b,e,d){c.b=e;c.a=d;cg(c,a,b);return c;}
function Ef(){}
_=Ef.prototype=new bg();_.tI=0;function mh(){mh=js;nh=(El(),am);}
var nh;function oi(a){a.jb(yb());mk(a,131197);lk(a,'gwt-Label');return a;}
function qi(a){switch(jc(a)){case 1:break;case 4:case 8:case 64:case 16:case 32:break;case 131072:break;}}
function ni(){}
_=ni.prototype=new yk();_.E=qi;_.tI=20;function xh(a){oi(a);a.jb(yb());mk(a,125);lk(a,'gwt-HTML');return a;}
function yh(b,a){xh(b);Ah(b,a);return b;}
function Ah(b,a){Ac(b.m,a);}
function wh(){}
_=wh.prototype=new ni();_.tI=21;function bi(){bi=js;Fh(new Eh(),'center');ci=Fh(new Eh(),'left');Fh(new Eh(),'right');}
var ci;function Fh(b,a){b.a=a;return b;}
function Eh(){}
_=Eh.prototype=new ym();_.tI=0;_.a=null;function ii(){ii=js;gi(new fi(),'bottom');gi(new fi(),'middle');ji=gi(new fi(),'top');}
var ji;function gi(a,b){a.a=b;return a;}
function fi(){}
_=fi.prototype=new ym();_.tI=0;_.a=null;function Ej(){Ej=js;El(),bm;}
function Dj(b,a){El(),bm;ph(b,a);mk(b,1024);return b;}
function Fj(a){var b;rh(this,a);b=jc(a);}
function Cj(){}
_=Cj.prototype=new oh();_.E=Fj;_.tI=22;function Bi(){Bi=js;El(),bm;}
function Ai(a){El(),bm;Dj(a,zb());lk(a,'gwt-PasswordTextBox');return a;}
function zi(){}
_=zi.prototype=new Cj();_.tI=23;function Fi(){Fi=js;El(),bm;}
function Di(a){{lk(a,'gwt-PushButton');}}
function Ei(b,c,a){El(),bm;jg(b,c,a);Di(b);return b;}
function cj(){Dg(this,false);}
function aj(){Dg(this,false);}
function bj(){Dg(this,true);}
function Ci(){}
_=Ci.prototype=new Df();_.bb=cj;_.F=aj;_.ab=bj;_.tI=24;function gj(){gj=js;El(),bm;}
function ej(b,a){El(),bm;mf(b,Ab(a));lk(b,'gwt-RadioButton');return b;}
function fj(c,b,a){El(),bm;ej(c,b);qf(c,a);return c;}
function dj(){}
_=dj.prototype=new lf();_.tI=25;function nj(){nj=js;sj=kr(new rq());}
function mj(b,a){nj();De(b);if(a===null){a=oj();}b.jb(a);b.D();return b;}
function pj(){nj();return qj(null);}
function qj(c){nj();var a,b;b=lb(qr(sj,c),6);if(b!==null){return b;}a=null;if(sj.c==0){rj();}rr(sj,c,b=mj(new hj(),a));return b;}
function oj(){nj();return $doc.body;}
function rj(){nj();sd(new ij());}
function hj(){}
_=hj.prototype=new Ce();_.tI=26;var sj;function kj(){var a,b;for(b=bp(pp((nj(),sj)));ip(b);){a=lb(jp(b),6);if(a.k){a.cb();}}}
function lj(){return null;}
function ij(){}
_=ij.prototype=new ym();_.fb=kj;_.gb=lj;_.tI=27;function bk(){bk=js;El(),bm;}
function ak(a){El(),bm;Dj(a,Bb());lk(a,'gwt-TextBox');return a;}
function Bj(){}
_=Bj.prototype=new Cj();_.tI=28;function sk(a){a.a=(bi(),ci);a.b=(ii(),ji);}
function tk(a){gf(a);sk(a);yc(a.d,'cellSpacing','0');yc(a.d,'cellPadding','0');return a;}
function uk(b,d){var a,c;c=ac();a=wk(b);wb(c,a);wb(b.c,c);yf(b,d,a);}
function wk(b){var a;a=Fb();jf(b,a,b.a);kf(b,a,b.b);return a;}
function xk(c){var a,b;b=oc(c.m);a=Af(this,c);if(a){tc(this.c,oc(b));}return a;}
function rk(){}
_=rk.prototype=new ff();_.ib=xk;_.tI=29;function bl(b,a){b.a=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[4],null);return b;}
function cl(a,b){fl(a,b,a.b);}
function el(b,c){var a;for(a=0;a<b.b;++a){if(b.a[a]===c){return a;}}return (-1);}
function fl(d,e,a){var b,c;if(a<0||a>d.b){throw new tm();}if(d.b==d.a.a){c=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[d.a.a*2],null);for(b=0;b<d.a.a;++b){gb(c,b,d.a[b]);}d.a=c;}++d.b;for(b=d.b-1;b>a;--b){gb(d.a,b,d.a[b-1]);}gb(d.a,a,e);}
function gl(a){return Bk(new Ak(),a);}
function hl(c,b){var a;if(b<0||b>=c.b){throw new tm();}--c.b;for(a=b;a<c.b;++a){gb(c.a,a,c.a[a+1]);}gb(c.a,c.b,null);}
function il(b,c){var a;a=el(b,c);if(a==(-1)){throw new fs();}hl(b,a);}
function zk(){}
_=zk.prototype=new ym();_.tI=0;_.a=null;_.b=0;function Bk(b,a){b.b=a;return b;}
function Dk(a){return a.a<a.b.b-1;}
function Ek(a){if(a.a>=a.b.b){throw new fs();}return a.b.a[++a.a];}
function Fk(){return Dk(this);}
function al(){return Ek(this);}
function Ak(){}
_=Ak.prototype=new ym();_.y=Fk;_.C=al;_.tI=0;_.a=(-1);function El(){El=js;am=zl(new yl());bm=am;}
function Cl(a){El();return a;}
function Dl(b,a){a.blur();}
function Fl(b){var a=$doc.createElement('DIV');a.tabIndex=0;return a;}
function xl(){}
_=xl.prototype=new ym();_.tI=0;var am,bm;function Al(){Al=js;El();}
function zl(a){Al();Cl(a);return a;}
function Bl(c,b){try{b.focus();}catch(a){if(!b|| !b.focus){throw a;}}}
function yl(){}
_=yl.prototype=new xl();_.tI=0;function sn(b,a){a;return b;}
function rn(){}
_=rn.prototype=new ym();_.tI=3;function lm(b,a){sn(b,a);return b;}
function km(){}
_=km.prototype=new rn();_.tI=4;function Dm(b,a){lm(b,a);return b;}
function Cm(){}
_=Cm.prototype=new km();_.tI=5;function dm(){}
_=dm.prototype=new Cm();_.tI=30;function gm(){}
_=gm.prototype=new Cm();_.tI=31;function om(b,a){Dm(b,a);return b;}
function nm(){}
_=nm.prototype=new Cm();_.tI=32;function rm(b,a){Dm(b,a);return b;}
function qm(){}
_=qm.prototype=new Cm();_.tI=33;function um(b,a){Dm(b,a);return b;}
function tm(){}
_=tm.prototype=new Cm();_.tI=34;function wm(){}
_=wm.prototype=new Cm();_.tI=35;function an(b,a){return b.charCodeAt(a);}
function cn(b,a){return b.indexOf(String.fromCharCode(a));}
function dn(b,a){return b.indexOf(a);}
function en(c,b,a){return c.indexOf(b,a);}
function fn(a){return a.length;}
function gn(b,a){return b.substr(a,b.length-a);}
function hn(c,a,b){return c.substr(a,b-a);}
function jn(c){var a=c.replace(/^(\s*)/,'');var b=a.replace(/\s*$/,'');return b;}
function kn(a,b){return String(a)==b;}
function ln(a){if(!mb(a,1))return false;return kn(this,a);}
function nn(){var a=mn;if(!a){a=mn={};}var e=':'+this;var b=a[e];if(b==null){b=0;var f=this.length;var d=f<64?1:f/32|0;for(var c=0;c<f;c+=d){b<<=1;b+=this.charCodeAt(c);}b|=0;a[e]=b;}return b;}
_=String.prototype;_.eQ=ln;_.hC=nn;_.tI=2;var mn=null;function qn(a){return s(a);}
function vn(b,a){Dm(b,a);return b;}
function un(){}
_=un.prototype=new Cm();_.tI=36;function yn(d,a,b){var c;while(a.y()){c=a.C();if(b===null?c===null:b.eQ(c)){return a;}}return null;}
function An(a){throw vn(new un(),'add');}
function Bn(b){var a;a=yn(this,this.A(),b);return a!==null;}
function xn(){}
_=xn.prototype=new ym();_.o=An;_.q=Bn;_.tI=0;function ho(b,a){throw um(new tm(),'Index: '+a+', Size: '+b.b);}
function io(a){return En(new Dn(),a);}
function jo(b,a){throw vn(new un(),'add');}
function ko(a){this.n(this.kb(),a);return true;}
function lo(e){var a,b,c,d,f;if(e===this){return true;}if(!mb(e,11)){return false;}f=lb(e,11);if(this.kb()!=f.kb()){return false;}c=io(this);d=f.A();while(ao(c)){a=bo(c);b=bo(d);if(!(a===null?b===null:a.eQ(b))){return false;}}return true;}
function mo(){var a,b,c,d;c=1;a=31;b=io(this);while(ao(b)){d=bo(b);c=31*c+(d===null?0:d.hC());}return c;}
function no(){return io(this);}
function oo(a){throw vn(new un(),'remove');}
function Cn(){}
_=Cn.prototype=new xn();_.n=jo;_.o=ko;_.eQ=lo;_.hC=mo;_.A=no;_.hb=oo;_.tI=37;function En(b,a){b.c=a;return b;}
function ao(a){return a.a<a.c.kb();}
function bo(a){if(!ao(a)){throw new fs();}return a.c.w(a.b=a.a++);}
function co(a){if(a.b<0){throw new qm();}a.c.hb(a.b);a.a=a.b;a.b=(-1);}
function eo(){return ao(this);}
function fo(){return bo(this);}
function Dn(){}
_=Dn.prototype=new ym();_.y=eo;_.C=fo;_.tI=0;_.a=0;_.b=(-1);function np(f,d,e){var a,b,c;for(b=fr(f.t());Eq(b);){a=Fq(b);c=a.u();if(d===null?c===null:d.eQ(c)){if(e){ar(b);}return a;}}return null;}
function op(b){var a;a=b.t();return ro(new qo(),b,a);}
function pp(b){var a;a=pr(b);return Fo(new Eo(),b,a);}
function qp(a){return np(this,a,false)!==null;}
function rp(d){var a,b,c,e,f,g,h;if(d===this){return true;}if(!mb(d,12)){return false;}f=lb(d,12);c=op(this);e=f.B();if(!xp(c,e)){return false;}for(a=to(c);Ao(a);){b=Bo(a);h=this.x(b);g=f.x(b);if(h===null?g!==null:!h.eQ(g)){return false;}}return true;}
function sp(b){var a;a=np(this,b,false);return a===null?null:a.v();}
function tp(){var a,b,c;b=0;for(c=fr(this.t());Eq(c);){a=Fq(c);b+=a.hC();}return b;}
function up(){return op(this);}
function po(){}
_=po.prototype=new ym();_.p=qp;_.eQ=rp;_.x=sp;_.hC=tp;_.B=up;_.tI=38;function xp(e,b){var a,c,d;if(b===e){return true;}if(!mb(b,13)){return false;}c=lb(b,13);if(c.kb()!=e.kb()){return false;}for(a=c.A();a.y();){d=a.C();if(!e.q(d)){return false;}}return true;}
function yp(a){return xp(this,a);}
function zp(){var a,b,c;a=0;for(b=this.A();b.y();){c=b.C();if(c!==null){a+=c.hC();}}return a;}
function vp(){}
_=vp.prototype=new xn();_.eQ=yp;_.hC=zp;_.tI=39;function ro(b,a,c){b.a=a;b.b=c;return b;}
function to(b){var a;a=fr(b.b);return yo(new xo(),b,a);}
function uo(a){return this.a.p(a);}
function vo(){return to(this);}
function wo(){return this.b.a.c;}
function qo(){}
_=qo.prototype=new vp();_.q=uo;_.A=vo;_.kb=wo;_.tI=40;function yo(b,a,c){b.a=c;return b;}
function Ao(a){return a.a.y();}
function Bo(b){var a;a=b.a.C();return a.u();}
function Co(){return Ao(this);}
function Do(){return Bo(this);}
function xo(){}
_=xo.prototype=new ym();_.y=Co;_.C=Do;_.tI=0;function Fo(b,a,c){b.a=a;b.b=c;return b;}
function bp(b){var a;a=fr(b.b);return gp(new fp(),b,a);}
function cp(a){return or(this.a,a);}
function dp(){return bp(this);}
function ep(){return this.b.a.c;}
function Eo(){}
_=Eo.prototype=new xn();_.q=cp;_.A=dp;_.kb=ep;_.tI=0;function gp(b,a,c){b.a=c;return b;}
function ip(a){return a.a.y();}
function jp(a){var b;b=a.a.C().v();return b;}
function kp(){return ip(this);}
function lp(){return jp(this);}
function fp(){}
_=fp.prototype=new ym();_.y=kp;_.C=lp;_.tI=0;function Bp(a){{Ep(a);}}
function Cp(a){Bp(a);return a;}
function Dp(b,a){nq(b.a,b.b++,a);return true;}
function Ep(a){a.a=z();a.b=0;}
function aq(b,a){if(a<0||a>=b.b){ho(b,a);}return jq(b.a,a);}
function bq(b,a){return cq(b,a,0);}
function cq(c,b,a){if(a<0){ho(c,a);}for(;a<c.b;++a){if(iq(b,jq(c.a,a))){return a;}}return (-1);}
function dq(c,a){var b;b=aq(c,a);lq(c.a,a,1);--c.b;return b;}
function fq(a,b){if(a<0||a>this.b){ho(this,a);}eq(this.a,a,b);++this.b;}
function gq(a){return Dp(this,a);}
function eq(a,b,c){a.splice(b,0,c);}
function hq(a){return bq(this,a)!=(-1);}
function iq(a,b){return a===b||a!==null&&a.eQ(b);}
function kq(a){return aq(this,a);}
function jq(a,b){return a[b];}
function mq(a){return dq(this,a);}
function lq(a,c,b){a.splice(c,b);}
function nq(a,b,c){a[b]=c;}
function oq(){return this.b;}
function Ap(){}
_=Ap.prototype=new Cn();_.n=fq;_.o=gq;_.q=hq;_.w=kq;_.hb=mq;_.kb=oq;_.tI=41;_.a=null;_.b=0;function mr(){mr=js;tr=zr();}
function jr(a){{lr(a);}}
function kr(a){mr();jr(a);return a;}
function lr(a){a.a=z();a.d=A();a.b=rb(tr,v);a.c=0;}
function nr(b,a){if(mb(a,1)){return Dr(b.d,lb(a,1))!==tr;}else if(a===null){return b.b!==tr;}else{return Cr(b.a,a,a.hC())!==tr;}}
function or(a,b){if(a.b!==tr&&Br(a.b,b)){return true;}else if(yr(a.d,b)){return true;}else if(wr(a.a,b)){return true;}return false;}
function pr(a){return dr(new Aq(),a);}
function qr(c,a){var b;if(mb(a,1)){b=Dr(c.d,lb(a,1));}else if(a===null){b=c.b;}else{b=Cr(c.a,a,a.hC());}return b===tr?null:b;}
function rr(c,a,d){var b;{b=c.b;c.b=d;}if(b===tr){++c.c;return null;}else{return b;}}
function sr(c,a){var b;if(mb(a,1)){b=as(c.d,lb(a,1));}else if(a===null){b=c.b;c.b=rb(tr,v);}else{b=Fr(c.a,a,a.hC());}if(b===tr){return null;}else{--c.c;return b;}}
function ur(e,c){mr();for(var d in e){if(d==parseInt(d)){var a=e[d];for(var f=0,b=a.length;f<b;++f){c.o(a[f]);}}}}
function vr(d,a){mr();for(var c in d){if(c.charCodeAt(0)==58){var e=d[c];var b=vq(c.substring(1),e);a.o(b);}}}
function wr(f,h){mr();for(var e in f){if(e==parseInt(e)){var a=f[e];for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.v();if(Br(h,d)){return true;}}}}return false;}
function xr(a){return nr(this,a);}
function yr(c,d){mr();for(var b in c){if(b.charCodeAt(0)==58){var a=c[b];if(Br(d,a)){return true;}}}return false;}
function zr(){mr();}
function Ar(){return pr(this);}
function Br(a,b){mr();if(a===b){return true;}else if(a===null){return false;}else{return a.eQ(b);}}
function Er(a){return qr(this,a);}
function Cr(f,h,e){mr();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.u();if(Br(h,d)){return c.v();}}}}
function Dr(b,a){mr();return b[':'+a];}
function Fr(f,h,e){mr();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.u();if(Br(h,d)){if(a.length==1){delete f[e];}else{a.splice(g,1);}return c.v();}}}}
function as(c,a){mr();a=':'+a;var b=c[a];delete c[a];return b;}
function rq(){}
_=rq.prototype=new po();_.p=xr;_.t=Ar;_.x=Er;_.tI=42;_.a=null;_.b=null;_.c=0;_.d=null;var tr;function tq(b,a,c){b.a=a;b.b=c;return b;}
function vq(a,b){return tq(new sq(),a,b);}
function wq(b){var a;if(mb(b,14)){a=lb(b,14);if(Br(this.a,a.u())&&Br(this.b,a.v())){return true;}}return false;}
function xq(){return this.a;}
function yq(){return this.b;}
function zq(){var a,b;a=0;b=0;if(this.a!==null){a=this.a.hC();}if(this.b!==null){b=this.b.hC();}return a^b;}
function sq(){}
_=sq.prototype=new ym();_.eQ=wq;_.u=xq;_.v=yq;_.hC=zq;_.tI=43;_.a=null;_.b=null;function dr(b,a){b.a=a;return b;}
function fr(a){return Cq(new Bq(),a.a);}
function gr(c){var a,b,d;if(mb(c,14)){a=lb(c,14);b=a.u();if(nr(this.a,b)){d=qr(this.a,b);return Br(a.v(),d);}}return false;}
function hr(){return fr(this);}
function ir(){return this.a.c;}
function Aq(){}
_=Aq.prototype=new vp();_.q=gr;_.A=hr;_.kb=ir;_.tI=44;function Cq(c,b){var a;c.c=b;a=Cp(new Ap());if(c.c.b!==(mr(),tr)){Dp(a,tq(new sq(),null,c.c.b));}vr(c.c.d,a);ur(c.c.a,a);c.a=io(a);return c;}
function Eq(a){return ao(a.a);}
function Fq(a){return a.b=lb(bo(a.a),14);}
function ar(a){if(a.b===null){throw rm(new qm(),'Must call next() before remove().');}else{co(a.a);sr(a.c,a.b.u());a.b=null;}}
function br(){return Eq(this);}
function cr(){return Fq(this);}
function Bq(){}
_=Bq.prototype=new ym();_.y=br;_.C=cr;_.tI=0;_.a=null;_.b=null;function fs(){}
_=fs.prototype=new Cm();_.tI=45;function ls(a){a.e=ak(new Bj());a.c=Ai(new zi());a.b=fj(new dj(),'interfaceRadioGroup','MOD Curator');a.d=fj(new dj(),'interfaceRadioGroup','Reference Genome');a.a=Ei(new Ci(),'Login','Login');}
function ms(a){ls(a);return a;}
function os(a){var b;b=tk(new rk());uk(b,yh(new wh(),'<h2>Login<\/h2>'));uk(b,a.e);uk(b,a.c);uk(b,a.d);uk(b,a.b);uk(b,a.a);return b;}
function ks(){}
_=ks.prototype=new ym();_.tI=0;function rs(b){var a;a=ms(new ks());Ee(pj(),os(a));}
function ps(){}
_=ps.prototype=new ym();_.tI=0;function cm(){rs(new ps());}
function gwtOnLoad(b,d,c){$moduleName=d;$moduleBase=c;if(b)try{cm();}catch(a){b(d);}else{cm();}}
var qb=[{},{},{1:1},{3:1},{3:1},{3:1},{3:1},{2:1},{2:1,4:1},{2:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{6:1,7:1,8:1,9:1,10:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{11:1},{12:1},{13:1},{13:1},{11:1},{12:1},{14:1},{13:1},{3:1}];if (org_bbop_RefGenome) {  var __gwt_initHandlers = org_bbop_RefGenome.__gwt_initHandlers;  org_bbop_RefGenome.onScriptLoad(gwtOnLoad);}})();