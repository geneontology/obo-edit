(function(){var $wnd = window;var $doc = $wnd.document;var $moduleName, $moduleBase;var _,Fs='com.google.gwt.core.client.',at='com.google.gwt.lang.',bt='com.google.gwt.user.client.',ct='com.google.gwt.user.client.impl.',dt='com.google.gwt.user.client.ui.',et='com.google.gwt.user.client.ui.impl.',ft='java.lang.',gt='java.util.',ht='org.bbop.client.',it='org.bbop.client.Pages.RefGenome.';function vs(){}
function gn(a){return this===a;}
function hn(){return Cn(this);}
function en(){}
_=en.prototype={};_.eQ=gn;_.hC=hn;_.tI=1;var o=null;function r(a){return a==null?0:a.$H?a.$H:(a.$H=t());}
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
_=v.prototype=new en();_.eQ=C;_.hC=D;_.tI=7;function F(c,a,d,b,e){c.a=a;c.b=b;e;c.tI=d;return c;}
function bb(a,b,c){return a[b]=c;}
function cb(b,a){return b[a];}
function db(a){return a.length;}
function fb(e,d,c,b,a){return eb(e,d,c,b,0,db(b),a);}
function eb(j,i,g,c,e,a,b){var d,f,h;if((f=cb(c,e))<0){throw new cn();}h=F(new E(),f,cb(i,e),cb(g,e),j);++e;if(e<a){j=tn(j,1);for(d=0;d<f;++d){bb(h,d,eb(j,i,g,c,e,a,b));}}else{for(d=0;d<f;++d){bb(h,d,b);}}return h;}
function gb(a,b,c){if(c!==null&&a.b!=0&& !mb(c,a.b)){throw new pm();}return bb(a,b,c);}
function E(){}
_=E.prototype=new en();_.tI=0;function jb(b,a){return !(!(b&&qb[b][a]));}
function kb(a){return String.fromCharCode(a);}
function lb(b,a){if(b!=null)jb(b.tI,a)||pb();return b;}
function mb(b,a){return b!=null&&jb(b.tI,a);}
function nb(a){return a&65535;}
function pb(){throw new sm();}
function ob(a){if(a!==null){throw new sm();}return a;}
function rb(b,d){_=d.prototype;if(b&& !(b.tI>=_.tI)){var c=b.toString;for(var a in _){b[a]=_[a];}b.toString=c;}return b;}
var qb;function vb(){vb=vs;vc=iq(new gq());{pc=new Ed();de(pc);}}
function wb(b,a){vb();se(pc,b,a);}
function xb(a,b){vb();return be(pc,a,b);}
function yb(){vb();return ue(pc,'div');}
function zb(){vb();return ve(pc,'password');}
function Ab(a){vb();return ke(pc,a);}
function Bb(){vb();return ve(pc,'text');}
function Cb(){vb();return ue(pc,'label');}
function Db(){vb();return ue(pc,'span');}
function Eb(){vb();return ue(pc,'tbody');}
function Fb(){vb();return ue(pc,'td');}
function ac(){vb();return ue(pc,'tr');}
function bc(){vb();return ue(pc,'table');}
function ec(b,a,d){vb();var c;c=o;{dc(b,a,d);}}
function dc(b,a,c){vb();var d;if(a===uc){if(jc(b)==8192){uc=null;}}d=cc;cc=b;try{c.F(b);}finally{cc=d;}}
function fc(b,a){vb();we(pc,b,a);}
function gc(a){vb();return xe(pc,a);}
function hc(a){vb();return le(pc,a);}
function ic(a){vb();return me(pc,a);}
function jc(a){vb();return ye(pc,a);}
function kc(a){vb();ne(pc,a);}
function mc(a,b){vb();return Ae(pc,a,b);}
function lc(a,b){vb();return ze(pc,a,b);}
function nc(a){vb();return Be(pc,a);}
function oc(a){vb();return oe(pc,a);}
function qc(b,a){vb();return ee(pc,b,a);}
function rc(a){vb();var b,c;c=true;if(vc.b>0){b=ob(mq(vc,vc.b-1));if(!(c=null.nb())){fc(a,true);kc(a);}}return c;}
function sc(a){vb();if(uc!==null&&xb(a,uc)){uc=null;}fe(pc,a);}
function tc(b,a){vb();Ce(pc,b,a);}
function wc(a){vb();uc=a;qe(pc,a);}
function yc(a,b,c){vb();Ee(pc,a,b,c);}
function xc(a,b,c){vb();De(pc,a,b,c);}
function zc(a,b){vb();Fe(pc,a,b);}
function Ac(a,b){vb();af(pc,a,b);}
function Bc(a,b){vb();bf(pc,a,b);}
function Cc(b,a,c){vb();cf(pc,b,a,c);}
function Dc(a,b){vb();he(pc,a,b);}
var cc=null,pc=null,uc=null,vc;function ad(a){if(mb(a,4)){return xb(this,lb(a,4));}return x(rb(this,Ec),a);}
function bd(){return y(rb(this,Ec));}
function Ec(){}
_=Ec.prototype=new v();_.eQ=ad;_.hC=bd;_.tI=8;function fd(a){return x(rb(this,cd),a);}
function gd(){return y(rb(this,cd));}
function cd(){}
_=cd.prototype=new v();_.eQ=fd;_.hC=gd;_.tI=9;function md(){md=vs;od=iq(new gq());{nd();}}
function nd(){md();sd(new id());}
var od;function kd(){while((md(),od).b>0){ob(mq((md(),od),0)).nb();}}
function ld(){return null;}
function id(){}
_=id.prototype=new en();_.gb=kd;_.hb=ld;_.tI=10;function rd(){rd=vs;td=iq(new gq());Bd=iq(new gq());{xd();}}
function sd(a){rd();jq(td,a);}
function ud(){rd();var a,b;for(a=uo(td);no(a);){b=lb(oo(a),5);b.gb();}}
function vd(){rd();var a,b,c,d;d=null;for(a=uo(td);no(a);){b=lb(oo(a),5);c=b.hb();{d=c;}}return d;}
function wd(){rd();var a,b;for(a=uo(Bd);no(a);){b=ob(oo(a));null.nb();}}
function xd(){rd();__gwt_initHandlers(function(){Ad();},function(){return zd();},function(){yd();$wnd.onresize=null;$wnd.onbeforeclose=null;$wnd.onclose=null;});}
function yd(){rd();var a;a=o;{ud();}}
function zd(){rd();var a;a=o;{return vd();}}
function Ad(){rd();var a;a=o;{wd();}}
var td,Bd;function se(c,b,a){b.appendChild(a);}
function ue(b,a){return $doc.createElement(a);}
function ve(b,c){var a=$doc.createElement('INPUT');a.type=c;return a;}
function we(c,b,a){b.cancelBubble=a;}
function xe(b,a){return a.which||(a.keyCode|| -1);}
function ye(b,a){switch(a.type){case 'blur':return 4096;case 'change':return 1024;case 'click':return 1;case 'dblclick':return 2;case 'focus':return 2048;case 'keydown':return 128;case 'keypress':return 256;case 'keyup':return 512;case 'load':return 32768;case 'losecapture':return 8192;case 'mousedown':return 4;case 'mousemove':return 64;case 'mouseout':return 32;case 'mouseover':return 16;case 'mouseup':return 8;case 'scroll':return 16384;case 'error':return 65536;case 'mousewheel':return 131072;case 'DOMMouseScroll':return 131072;}}
function Ae(d,a,b){var c=a[b];return c==null?null:String(c);}
function ze(c,a,b){return !(!a[b]);}
function Be(b,a){return a.__eventBits||0;}
function Ce(c,b,a){b.removeChild(a);}
function Ee(c,a,b,d){a[b]=d;}
function De(c,a,b,d){a[b]=d;}
function Fe(c,a,b){a.__listener=b;}
function af(c,a,b){if(!b){b='';}a.innerHTML=b;}
function bf(c,a,b){while(a.firstChild){a.removeChild(a.firstChild);}if(b!=null){a.appendChild($doc.createTextNode(b));}}
function cf(c,b,a,d){b.style[a]=d;}
function Cd(){}
_=Cd.prototype=new en();_.tI=0;function ke(c,b){var a=$doc.createElement('INPUT');a.type='radio';a.name=b;return a;}
function le(b,a){return a.target||null;}
function me(b,a){return a.relatedTarget||null;}
function ne(b,a){a.preventDefault();}
function oe(c,a){var b=a.parentNode;if(b==null){return null;}if(b.nodeType!=1)b=null;return b||null;}
function pe(d){$wnd.__dispatchCapturedMouseEvent=function(b){if($wnd.__dispatchCapturedEvent(b)){var a=$wnd.__captureElem;if(a&&a.__listener){ec(b,a,a.__listener);b.stopPropagation();}}};$wnd.__dispatchCapturedEvent=function(a){if(!rc(a)){a.stopPropagation();a.preventDefault();return false;}return true;};$wnd.addEventListener('click',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('dblclick',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousedown',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mouseup',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousemove',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousewheel',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('keydown',$wnd.__dispatchCapturedEvent,true);$wnd.addEventListener('keyup',$wnd.__dispatchCapturedEvent,true);$wnd.addEventListener('keypress',$wnd.__dispatchCapturedEvent,true);$wnd.__dispatchEvent=function(b){var c,a=this;while(a&& !(c=a.__listener))a=a.parentNode;if(a&&a.nodeType!=1)a=null;if(c)ec(b,a,c);};$wnd.__captureElem=null;}
function qe(b,a){$wnd.__captureElem=a;}
function re(c,b,a){b.__eventBits=a;b.onclick=a&1?$wnd.__dispatchEvent:null;b.ondblclick=a&2?$wnd.__dispatchEvent:null;b.onmousedown=a&4?$wnd.__dispatchEvent:null;b.onmouseup=a&8?$wnd.__dispatchEvent:null;b.onmouseover=a&16?$wnd.__dispatchEvent:null;b.onmouseout=a&32?$wnd.__dispatchEvent:null;b.onmousemove=a&64?$wnd.__dispatchEvent:null;b.onkeydown=a&128?$wnd.__dispatchEvent:null;b.onkeypress=a&256?$wnd.__dispatchEvent:null;b.onkeyup=a&512?$wnd.__dispatchEvent:null;b.onchange=a&1024?$wnd.__dispatchEvent:null;b.onfocus=a&2048?$wnd.__dispatchEvent:null;b.onblur=a&4096?$wnd.__dispatchEvent:null;b.onlosecapture=a&8192?$wnd.__dispatchEvent:null;b.onscroll=a&16384?$wnd.__dispatchEvent:null;b.onload=a&32768?$wnd.__dispatchEvent:null;b.onerror=a&65536?$wnd.__dispatchEvent:null;b.onmousewheel=a&131072?$wnd.__dispatchEvent:null;}
function ie(){}
_=ie.prototype=new Cd();_.tI=0;function be(c,a,b){if(!a&& !b){return true;}else if(!a|| !b){return false;}return a.isSameNode(b);}
function de(a){pe(a);ce(a);}
function ce(d){$wnd.addEventListener('mouseout',function(b){var a=$wnd.__captureElem;if(a&& !b.relatedTarget){if('html'==b.target.tagName.toLowerCase()){var c=$doc.createEvent('MouseEvents');c.initMouseEvent('mouseup',true,true,$wnd,0,b.screenX,b.screenY,b.clientX,b.clientY,b.ctrlKey,b.altKey,b.shiftKey,b.metaKey,b.button,null);a.dispatchEvent(c);}}},true);$wnd.addEventListener('DOMMouseScroll',$wnd.__dispatchCapturedMouseEvent,true);}
function ee(d,c,b){while(b){if(c.isSameNode(b)){return true;}try{b=b.parentNode;}catch(a){return false;}if(b&&b.nodeType!=1){b=null;}}return false;}
function fe(b,a){if(a.isSameNode($wnd.__captureElem)){$wnd.__captureElem=null;}}
function he(c,b,a){re(c,b,a);ge(c,b,a);}
function ge(c,b,a){if(a&131072){b.addEventListener('DOMMouseScroll',$wnd.__dispatchEvent,false);}}
function Dd(){}
_=Dd.prototype=new ie();_.tI=0;function Ed(){}
_=Ed.prototype=new Dd();_.tI=0;function kk(b,a){lk(b,nk(b)+kb(45)+a);}
function lk(b,a){xk(b.m,a,true);}
function nk(a){return vk(a.m);}
function ok(b,a){pk(b,nk(b)+kb(45)+a);}
function pk(b,a){xk(b.m,a,false);}
function qk(d,b,a){var c=b.parentNode;if(!c){return;}c.insertBefore(a,b);c.removeChild(b);}
function rk(b,a){if(b.m!==null){qk(b,b.m,a);}b.m=a;}
function sk(b,a){wk(b.m,a);}
function tk(b,a){Dc(b.m,a|nc(b.m));}
function uk(a){return mc(a,'className');}
function vk(a){var b,c;b=uk(a);c=pn(b,32);if(c>=0){return un(b,0,c);}return b;}
function wk(a,b){yc(a,'className',b);}
function xk(c,j,a){var b,d,e,f,g,h,i;if(c===null){throw kn(new jn(),'Null widget handle. If you are creating a composite, ensure that initWidget() has been called.');}j=vn(j);if(sn(j)==0){throw Am(new zm(),'Style names cannot be empty');}i=uk(c);e=qn(i,j);while(e!=(-1)){if(e==0||nn(i,e-1)==32){f=e+sn(j);g=sn(i);if(f==g||f<g&&nn(i,f)==32){break;}}e=rn(i,j,e+1);}if(a){if(e==(-1)){if(sn(i)>0){i+=' ';}yc(c,'className',i+j);}}else{if(e!=(-1)){b=vn(un(i,0,e));d=vn(tn(i,e+sn(j)));if(sn(b)==0){h=d;}else if(sn(d)==0){h=b;}else{h=b+' '+d;}yc(c,'className',h);}}}
function jk(){}
_=jk.prototype=new en();_.tI=0;_.m=null;function rl(a){if(a.k){throw Dm(new Cm(),"Should only call onAttach when the widget is detached from the browser's document");}a.k=true;zc(a.m,a);a.s();a.eb();}
function sl(a){if(!a.k){throw Dm(new Cm(),"Should only call onDetach when the widget is attached to the browser's document");}try{a.fb();}finally{a.t();zc(a.m,null);a.k=false;}}
function tl(a){if(a.l!==null){a.l.jb(a);}else if(a.l!==null){throw Dm(new Cm(),"This widget's parent does not implement HasWidgets");}}
function ul(b,a){if(b.k){zc(b.m,null);}rk(b,a);if(b.k){zc(a,b);}}
function vl(c,b){var a;a=c.l;if(b===null){if(a!==null&&a.k){c.db();}c.l=null;}else{if(a!==null){throw Dm(new Cm(),'Cannot set a new parent without first clearing the old parent');}c.l=b;if(b.k){c.E();}}}
function wl(){}
function xl(){}
function yl(){rl(this);}
function zl(a){}
function Al(){sl(this);}
function Bl(){}
function Cl(){}
function Dl(a){ul(this,a);}
function Fk(){}
_=Fk.prototype=new jk();_.s=wl;_.t=xl;_.E=yl;_.F=zl;_.db=Al;_.eb=Bl;_.fb=Cl;_.kb=Dl;_.tI=11;_.k=false;_.l=null;function zi(b,a){vl(a,b);}
function Bi(b,a){vl(a,null);}
function Ci(){var a,b;for(b=this.B();el(b);){a=fl(b);a.E();}}
function Di(){var a,b;for(b=this.B();el(b);){a=fl(b);a.db();}}
function Ei(){}
function Fi(){}
function yi(){}
_=yi.prototype=new Fk();_.s=Ci;_.t=Di;_.eb=Ei;_.fb=Fi;_.tI=12;function Df(a){a.e=il(new al(),a);}
function Ef(a){Df(a);return a;}
function Ff(c,a,b){tl(a);jl(c.e,a);wb(b,a.m);zi(c,a);}
function bg(b,c){var a;if(c.l!==b){return false;}Bi(b,c);a=c.m;tc(oc(a),a);pl(b.e,c);return true;}
function cg(){return nl(this.e);}
function dg(a){return bg(this,a);}
function Cf(){}
_=Cf.prototype=new yi();_.B=cg;_.jb=dg;_.tI=13;function ef(a){Ef(a);a.kb(yb());Cc(a.m,'position','relative');Cc(a.m,'overflow','hidden');return a;}
function ff(a,b){Ff(a,b,a.m);}
function hf(a){Cc(a,'left','');Cc(a,'top','');Cc(a,'position','');}
function jf(b){var a;a=bg(this,b);if(a){hf(b.m);}return a;}
function df(){}
_=df.prototype=new Cf();_.jb=jf;_.tI=14;function xh(){xh=vs;lm(),nm;}
function wh(b,a){lm(),nm;zh(b,a);return b;}
function yh(b,a){switch(jc(a)){case 1:break;case 4096:case 2048:break;case 128:case 512:case 256:break;}}
function zh(b,a){ul(b,a);tk(b,7041);}
function Ah(){return !lc(this.m,'disabled');}
function Bh(a){yh(this,a);}
function Ch(a){zh(this,a);}
function vh(){}
_=vh.prototype=new Fk();_.A=Ah;_.F=Bh;_.kb=Ch;_.tI=15;function mf(){mf=vs;lm(),nm;}
function lf(b,a){lm(),nm;wh(b,a);return b;}
function kf(){}
_=kf.prototype=new vh();_.tI=16;function of(a){Ef(a);a.d=bc();a.c=Eb();wb(a.d,a.c);a.kb(a.d);return a;}
function qf(c,b,a){yc(b,'align',a.a);}
function rf(c,b,a){Cc(b,'verticalAlign',a.a);}
function nf(){}
_=nf.prototype=new Cf();_.tI=17;_.c=null;_.d=null;function uf(){uf=vs;lm(),nm;}
function tf(b,a){var c;lm(),nm;lf(b,Db());b.a=a;b.b=Cb();Dc(b.a,nc(b.m));Dc(b.m,0);wb(b.m,b.a);wb(b.m,b.b);c='check'+ ++Bf;yc(b.a,'id',c);yc(b.b,'htmlFor',c);return b;}
function vf(b){var a;a=b.k?'checked':'defaultChecked';return lc(b.a,a);}
function wf(b,a){xc(b.a,'checked',a);xc(b.a,'defaultChecked',a);}
function xf(b,a){Bc(b.b,a);}
function yf(){return !lc(this.a,'disabled');}
function zf(){zc(this.a,this);}
function Af(){zc(this.a,null);wf(this,vf(this));}
function sf(){}
_=sf.prototype=new kf();_.A=yf;_.eb=zf;_.fb=Af;_.tI=18;_.a=null;_.b=null;var Bf=0;function sg(){sg=vs;lm(),nm;}
function pg(a,b){lm(),nm;og(a);mg(a.h,b);return a;}
function qg(b,c,a){lm(),nm;pg(b,c);mg(xg(b),a);return b;}
function og(a){lm(),nm;lf(a,gm((th(),uh)));tk(a,6269);ih(a,tg(a,null,'up',0));sk(a,'gwt-CustomButton');return a;}
function rg(a){if(a.f||a.g){sc(a.m);a.f=false;a.g=false;a.ab();}}
function tg(d,a,c,b){return gg(new fg(),a,d,c,b);}
function ug(a){if(a.a===null){ah(a,a.h);}}
function vg(a){ug(a);return a.a;}
function wg(a){if(a.d===null){bh(a,tg(a,xg(a),'down-disabled',5));}return a.d;}
function xg(a){if(a.c===null){ch(a,tg(a,a.h,'down',1));}return a.c;}
function yg(a){if(a.e===null){dh(a,tg(a,xg(a),'down-hovering',3));}return a.e;}
function zg(b,a){switch(a){case 1:return xg(b);case 0:return b.h;case 3:return yg(b);case 2:return Bg(b);case 4:return Ag(b);case 5:return wg(b);default:throw Dm(new Cm(),a+' is not a known face id.');}}
function Ag(a){if(a.i===null){hh(a,tg(a,a.h,'up-disabled',4));}return a.i;}
function Bg(a){if(a.j===null){jh(a,tg(a,a.h,'up-hovering',2));}return a.j;}
function Cg(a){return (1&vg(a).a)>0;}
function Dg(a){return (2&vg(a).a)>0;}
function ah(b,a){if(b.a!==a){if(b.a!==null){ok(b,b.a.b);}b.a=a;Eg(b,lg(a));kk(b,b.a.b);}}
function Fg(c,a){var b;b=zg(c,a);ah(c,b);}
function Eg(b,a){if(b.b!==a){if(b.b!==null){tc(b.m,b.b);}b.b=a;wb(b.m,b.b);}}
function eh(b,a){if(a!=Cg(b)){kh(b);}}
function bh(b,a){b.d=a;}
function ch(b,a){b.c=a;}
function dh(b,a){b.e=a;}
function fh(b,a){if(a){im((th(),uh),b.m);}else{cm((th(),uh),b.m);}}
function gh(b,a){if(a!=Dg(b)){lh(b);}}
function hh(a,b){a.i=b;}
function ih(a,b){a.h=b;}
function jh(a,b){a.j=b;}
function kh(b){var a;a=vg(b).a^1;Fg(b,a);}
function lh(b){var a;a=vg(b).a^2;a&=(-5);Fg(b,a);}
function mh(){ug(this);rl(this);}
function nh(a){var b,c;if(this.A()==false){return;}c=jc(a);switch(c){case 4:fh(this,true);this.bb();wc(this.m);this.f=true;kc(a);break;case 8:if(this.f){this.f=false;sc(this.m);if(Dg(this)){this.cb();}}break;case 64:if(this.f){kc(a);}break;case 32:if(qc(this.m,hc(a))&& !qc(this.m,ic(a))){if(this.f){this.ab();}gh(this,false);}break;case 16:if(qc(this.m,hc(a))){gh(this,true);if(this.f){this.bb();}}break;case 1:return;case 4096:if(this.g){this.g=false;this.ab();}break;case 8192:if(this.f){this.f=false;this.ab();}break;}yh(this,a);b=nb(gc(a));switch(c){case 128:if(b==32){this.g=true;this.bb();}break;case 512:if(this.g&&b==32){this.g=false;this.cb();}break;case 256:if(b==10||b==13){this.bb();this.cb();}break;}}
function qh(){}
function oh(){}
function ph(){}
function rh(){sl(this);rg(this);}
function eg(){}
_=eg.prototype=new kf();_.E=mh;_.F=nh;_.cb=qh;_.ab=oh;_.bb=ph;_.db=rh;_.tI=19;_.a=null;_.b=null;_.c=null;_.d=null;_.e=null;_.f=false;_.g=false;_.h=null;_.i=null;_.j=null;function jg(c,a,b){c.e=b;c.c=a;return c;}
function lg(a){if(a.d===null){if(a.c===null){a.d=yb();return a.d;}else{return lg(a.c);}}else{return a.d;}}
function mg(b,a){b.d=yb();xk(b.d,'html-face',true);Bc(b.d,a);ng(b);}
function ng(a){if(a.e.a!==null&&lg(a.e.a)===lg(a)){Eg(a.e,a.d);}}
function ig(){}
_=ig.prototype=new en();_.tI=0;_.c=null;_.d=null;function gg(c,a,b,e,d){c.b=e;c.a=d;jg(c,a,b);return c;}
function fg(){}
_=fg.prototype=new ig();_.tI=0;function th(){th=vs;uh=(lm(),mm);}
var uh;function vi(a){a.kb(yb());tk(a,131197);sk(a,'gwt-Label');return a;}
function xi(a){switch(jc(a)){case 1:break;case 4:case 8:case 64:case 16:case 32:break;case 131072:break;}}
function ui(){}
_=ui.prototype=new Fk();_.F=xi;_.tI=20;function Eh(a){vi(a);a.kb(yb());tk(a,125);sk(a,'gwt-HTML');return a;}
function Fh(b,a){Eh(b);bi(b,a);return b;}
function bi(b,a){Ac(b.m,a);}
function Dh(){}
_=Dh.prototype=new ui();_.tI=21;function ii(){ii=vs;gi(new fi(),'center');ji=gi(new fi(),'left');gi(new fi(),'right');}
var ji;function gi(b,a){b.a=a;return b;}
function fi(){}
_=fi.prototype=new en();_.tI=0;_.a=null;function pi(){pi=vs;ni(new mi(),'bottom');ni(new mi(),'middle');qi=ni(new mi(),'top');}
var qi;function ni(a,b){a.a=b;return a;}
function mi(){}
_=mi.prototype=new en();_.tI=0;_.a=null;function fk(){fk=vs;lm(),nm;}
function ek(b,a){lm(),nm;wh(b,a);tk(b,1024);return b;}
function gk(a){var b;yh(this,a);b=jc(a);}
function dk(){}
_=dk.prototype=new vh();_.F=gk;_.tI=22;function cj(){cj=vs;lm(),nm;}
function bj(a){lm(),nm;ek(a,zb());sk(a,'gwt-PasswordTextBox');return a;}
function aj(){}
_=aj.prototype=new dk();_.tI=23;function gj(){gj=vs;lm(),nm;}
function ej(a){{sk(a,'gwt-PushButton');}}
function fj(b,c,a){lm(),nm;qg(b,c,a);ej(b);return b;}
function jj(){eh(this,false);}
function hj(){eh(this,false);}
function ij(){eh(this,true);}
function dj(){}
_=dj.prototype=new eg();_.cb=jj;_.ab=hj;_.bb=ij;_.tI=24;function nj(){nj=vs;lm(),nm;}
function lj(b,a){lm(),nm;tf(b,Ab(a));sk(b,'gwt-RadioButton');return b;}
function mj(c,b,a){lm(),nm;lj(c,b);xf(c,a);return c;}
function kj(){}
_=kj.prototype=new sf();_.tI=25;function uj(){uj=vs;zj=wr(new Dq());}
function tj(b,a){uj();ef(b);if(a===null){a=vj();}b.kb(a);b.E();return b;}
function wj(){uj();return xj(null);}
function xj(c){uj();var a,b;b=lb(Cr(zj,c),6);if(b!==null){return b;}a=null;if(zj.c==0){yj();}Dr(zj,c,b=tj(new oj(),a));return b;}
function vj(){uj();return $doc.body;}
function yj(){uj();sd(new pj());}
function oj(){}
_=oj.prototype=new df();_.tI=26;var zj;function rj(){var a,b;for(b=np(Bp((uj(),zj)));up(b);){a=lb(vp(b),6);if(a.k){a.db();}}}
function sj(){return null;}
function pj(){}
_=pj.prototype=new en();_.gb=rj;_.hb=sj;_.tI=27;function ik(){ik=vs;lm(),nm;}
function hk(a){lm(),nm;ek(a,Bb());sk(a,'gwt-TextBox');return a;}
function ck(){}
_=ck.prototype=new dk();_.tI=28;function zk(a){a.a=(ii(),ji);a.b=(pi(),qi);}
function Ak(a){of(a);zk(a);yc(a.d,'cellSpacing','0');yc(a.d,'cellPadding','0');return a;}
function Bk(b,d){var a,c;c=ac();a=Dk(b);wb(c,a);wb(b.c,c);Ff(b,d,a);}
function Dk(b){var a;a=Fb();qf(b,a,b.a);rf(b,a,b.b);return a;}
function Ek(c){var a,b;b=oc(c.m);a=bg(this,c);if(a){tc(this.c,oc(b));}return a;}
function yk(){}
_=yk.prototype=new nf();_.jb=Ek;_.tI=29;function il(b,a){b.a=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[4],null);return b;}
function jl(a,b){ml(a,b,a.b);}
function ll(b,c){var a;for(a=0;a<b.b;++a){if(b.a[a]===c){return a;}}return (-1);}
function ml(d,e,a){var b,c;if(a<0||a>d.b){throw new Fm();}if(d.b==d.a.a){c=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[d.a.a*2],null);for(b=0;b<d.a.a;++b){gb(c,b,d.a[b]);}d.a=c;}++d.b;for(b=d.b-1;b>a;--b){gb(d.a,b,d.a[b-1]);}gb(d.a,a,e);}
function nl(a){return cl(new bl(),a);}
function ol(c,b){var a;if(b<0||b>=c.b){throw new Fm();}--c.b;for(a=b;a<c.b;++a){gb(c.a,a,c.a[a+1]);}gb(c.a,c.b,null);}
function pl(b,c){var a;a=ll(b,c);if(a==(-1)){throw new rs();}ol(b,a);}
function al(){}
_=al.prototype=new en();_.tI=0;_.a=null;_.b=0;function cl(b,a){b.b=a;return b;}
function el(a){return a.a<a.b.b-1;}
function fl(a){if(a.a>=a.b.b){throw new rs();}return a.b.a[++a.a];}
function gl(){return el(this);}
function hl(){return fl(this);}
function bl(){}
_=bl.prototype=new en();_.z=gl;_.D=hl;_.tI=0;_.a=(-1);function lm(){lm=vs;mm=bm(new Fl());nm=mm!==null?km(new El()):mm;}
function km(a){lm();return a;}
function El(){}
_=El.prototype=new en();_.tI=0;var mm,nm;function dm(){dm=vs;lm();}
function am(a){a.a=em(a);a.b=fm(a);a.c=hm(a);}
function bm(a){dm();km(a);am(a);return a;}
function cm(b,a){a.firstChild.blur();}
function em(b){return function(a){if(this.parentNode.onblur){this.parentNode.onblur(a);}};}
function fm(b){return function(a){if(this.parentNode.onfocus){this.parentNode.onfocus(a);}};}
function gm(c){var a=$doc.createElement('div');var b=c.r();b.addEventListener('blur',c.a,false);b.addEventListener('focus',c.b,false);a.addEventListener('mousedown',c.c,false);a.appendChild(b);return a;}
function hm(a){return function(){this.firstChild.focus();};}
function im(b,a){a.firstChild.focus();}
function jm(){var a=$doc.createElement('input');a.type='text';a.style.width=a.style.height=0;a.style.zIndex= -1;a.style.position='absolute';return a;}
function Fl(){}
_=Fl.prototype=new El();_.r=jm;_.tI=0;function En(b,a){a;return b;}
function Dn(){}
_=Dn.prototype=new en();_.tI=3;function xm(b,a){En(b,a);return b;}
function wm(){}
_=wm.prototype=new Dn();_.tI=4;function kn(b,a){xm(b,a);return b;}
function jn(){}
_=jn.prototype=new wm();_.tI=5;function pm(){}
_=pm.prototype=new jn();_.tI=30;function sm(){}
_=sm.prototype=new jn();_.tI=31;function Am(b,a){kn(b,a);return b;}
function zm(){}
_=zm.prototype=new jn();_.tI=32;function Dm(b,a){kn(b,a);return b;}
function Cm(){}
_=Cm.prototype=new jn();_.tI=33;function an(b,a){kn(b,a);return b;}
function Fm(){}
_=Fm.prototype=new jn();_.tI=34;function cn(){}
_=cn.prototype=new jn();_.tI=35;function nn(b,a){return b.charCodeAt(a);}
function pn(b,a){return b.indexOf(String.fromCharCode(a));}
function qn(b,a){return b.indexOf(a);}
function rn(c,b,a){return c.indexOf(b,a);}
function sn(a){return a.length;}
function tn(b,a){return b.substr(a,b.length-a);}
function un(c,a,b){return c.substr(a,b-a);}
function vn(c){var a=c.replace(/^(\s*)/,'');var b=a.replace(/\s*$/,'');return b;}
function wn(a,b){return String(a)==b;}
function xn(a){if(!mb(a,1))return false;return wn(this,a);}
function zn(){var a=yn;if(!a){a=yn={};}var e=':'+this;var b=a[e];if(b==null){b=0;var f=this.length;var d=f<64?1:f/32|0;for(var c=0;c<f;c+=d){b<<=1;b+=this.charCodeAt(c);}b|=0;a[e]=b;}return b;}
_=String.prototype;_.eQ=xn;_.hC=zn;_.tI=2;var yn=null;function Cn(a){return s(a);}
function bo(b,a){kn(b,a);return b;}
function ao(){}
_=ao.prototype=new jn();_.tI=36;function fo(d,a,b){var c;while(a.z()){c=a.D();if(b===null?c===null:b.eQ(c)){return a;}}return null;}
function ho(a){throw bo(new ao(),'add');}
function io(b){var a;a=fo(this,this.B(),b);return a!==null;}
function eo(){}
_=eo.prototype=new en();_.o=ho;_.q=io;_.tI=0;function to(b,a){throw an(new Fm(),'Index: '+a+', Size: '+b.b);}
function uo(a){return lo(new ko(),a);}
function vo(b,a){throw bo(new ao(),'add');}
function wo(a){this.n(this.lb(),a);return true;}
function xo(e){var a,b,c,d,f;if(e===this){return true;}if(!mb(e,11)){return false;}f=lb(e,11);if(this.lb()!=f.lb()){return false;}c=uo(this);d=f.B();while(no(c)){a=oo(c);b=oo(d);if(!(a===null?b===null:a.eQ(b))){return false;}}return true;}
function yo(){var a,b,c,d;c=1;a=31;b=uo(this);while(no(b)){d=oo(b);c=31*c+(d===null?0:d.hC());}return c;}
function zo(){return uo(this);}
function Ao(a){throw bo(new ao(),'remove');}
function jo(){}
_=jo.prototype=new eo();_.n=vo;_.o=wo;_.eQ=xo;_.hC=yo;_.B=zo;_.ib=Ao;_.tI=37;function lo(b,a){b.c=a;return b;}
function no(a){return a.a<a.c.lb();}
function oo(a){if(!no(a)){throw new rs();}return a.c.x(a.b=a.a++);}
function po(a){if(a.b<0){throw new Cm();}a.c.ib(a.b);a.a=a.b;a.b=(-1);}
function qo(){return no(this);}
function ro(){return oo(this);}
function ko(){}
_=ko.prototype=new en();_.z=qo;_.D=ro;_.tI=0;_.a=0;_.b=(-1);function zp(f,d,e){var a,b,c;for(b=rr(f.u());kr(b);){a=lr(b);c=a.v();if(d===null?c===null:d.eQ(c)){if(e){mr(b);}return a;}}return null;}
function Ap(b){var a;a=b.u();return Do(new Co(),b,a);}
function Bp(b){var a;a=Br(b);return lp(new kp(),b,a);}
function Cp(a){return zp(this,a,false)!==null;}
function Dp(d){var a,b,c,e,f,g,h;if(d===this){return true;}if(!mb(d,12)){return false;}f=lb(d,12);c=Ap(this);e=f.C();if(!dq(c,e)){return false;}for(a=Fo(c);gp(a);){b=hp(a);h=this.y(b);g=f.y(b);if(h===null?g!==null:!h.eQ(g)){return false;}}return true;}
function Ep(b){var a;a=zp(this,b,false);return a===null?null:a.w();}
function Fp(){var a,b,c;b=0;for(c=rr(this.u());kr(c);){a=lr(c);b+=a.hC();}return b;}
function aq(){return Ap(this);}
function Bo(){}
_=Bo.prototype=new en();_.p=Cp;_.eQ=Dp;_.y=Ep;_.hC=Fp;_.C=aq;_.tI=38;function dq(e,b){var a,c,d;if(b===e){return true;}if(!mb(b,13)){return false;}c=lb(b,13);if(c.lb()!=e.lb()){return false;}for(a=c.B();a.z();){d=a.D();if(!e.q(d)){return false;}}return true;}
function eq(a){return dq(this,a);}
function fq(){var a,b,c;a=0;for(b=this.B();b.z();){c=b.D();if(c!==null){a+=c.hC();}}return a;}
function bq(){}
_=bq.prototype=new eo();_.eQ=eq;_.hC=fq;_.tI=39;function Do(b,a,c){b.a=a;b.b=c;return b;}
function Fo(b){var a;a=rr(b.b);return ep(new dp(),b,a);}
function ap(a){return this.a.p(a);}
function bp(){return Fo(this);}
function cp(){return this.b.a.c;}
function Co(){}
_=Co.prototype=new bq();_.q=ap;_.B=bp;_.lb=cp;_.tI=40;function ep(b,a,c){b.a=c;return b;}
function gp(a){return a.a.z();}
function hp(b){var a;a=b.a.D();return a.v();}
function ip(){return gp(this);}
function jp(){return hp(this);}
function dp(){}
_=dp.prototype=new en();_.z=ip;_.D=jp;_.tI=0;function lp(b,a,c){b.a=a;b.b=c;return b;}
function np(b){var a;a=rr(b.b);return sp(new rp(),b,a);}
function op(a){return Ar(this.a,a);}
function pp(){return np(this);}
function qp(){return this.b.a.c;}
function kp(){}
_=kp.prototype=new eo();_.q=op;_.B=pp;_.lb=qp;_.tI=0;function sp(b,a,c){b.a=c;return b;}
function up(a){return a.a.z();}
function vp(a){var b;b=a.a.D().w();return b;}
function wp(){return up(this);}
function xp(){return vp(this);}
function rp(){}
_=rp.prototype=new en();_.z=wp;_.D=xp;_.tI=0;function hq(a){{kq(a);}}
function iq(a){hq(a);return a;}
function jq(b,a){zq(b.a,b.b++,a);return true;}
function kq(a){a.a=z();a.b=0;}
function mq(b,a){if(a<0||a>=b.b){to(b,a);}return vq(b.a,a);}
function nq(b,a){return oq(b,a,0);}
function oq(c,b,a){if(a<0){to(c,a);}for(;a<c.b;++a){if(uq(b,vq(c.a,a))){return a;}}return (-1);}
function pq(c,a){var b;b=mq(c,a);xq(c.a,a,1);--c.b;return b;}
function rq(a,b){if(a<0||a>this.b){to(this,a);}qq(this.a,a,b);++this.b;}
function sq(a){return jq(this,a);}
function qq(a,b,c){a.splice(b,0,c);}
function tq(a){return nq(this,a)!=(-1);}
function uq(a,b){return a===b||a!==null&&a.eQ(b);}
function wq(a){return mq(this,a);}
function vq(a,b){return a[b];}
function yq(a){return pq(this,a);}
function xq(a,c,b){a.splice(c,b);}
function zq(a,b,c){a[b]=c;}
function Aq(){return this.b;}
function gq(){}
_=gq.prototype=new jo();_.n=rq;_.o=sq;_.q=tq;_.x=wq;_.ib=yq;_.lb=Aq;_.tI=41;_.a=null;_.b=0;function yr(){yr=vs;Fr=fs();}
function vr(a){{xr(a);}}
function wr(a){yr();vr(a);return a;}
function xr(a){a.a=z();a.d=A();a.b=rb(Fr,v);a.c=0;}
function zr(b,a){if(mb(a,1)){return js(b.d,lb(a,1))!==Fr;}else if(a===null){return b.b!==Fr;}else{return is(b.a,a,a.hC())!==Fr;}}
function Ar(a,b){if(a.b!==Fr&&hs(a.b,b)){return true;}else if(es(a.d,b)){return true;}else if(cs(a.a,b)){return true;}return false;}
function Br(a){return pr(new gr(),a);}
function Cr(c,a){var b;if(mb(a,1)){b=js(c.d,lb(a,1));}else if(a===null){b=c.b;}else{b=is(c.a,a,a.hC());}return b===Fr?null:b;}
function Dr(c,a,d){var b;{b=c.b;c.b=d;}if(b===Fr){++c.c;return null;}else{return b;}}
function Er(c,a){var b;if(mb(a,1)){b=ms(c.d,lb(a,1));}else if(a===null){b=c.b;c.b=rb(Fr,v);}else{b=ls(c.a,a,a.hC());}if(b===Fr){return null;}else{--c.c;return b;}}
function as(e,c){yr();for(var d in e){if(d==parseInt(d)){var a=e[d];for(var f=0,b=a.length;f<b;++f){c.o(a[f]);}}}}
function bs(d,a){yr();for(var c in d){if(c.charCodeAt(0)==58){var e=d[c];var b=br(c.substring(1),e);a.o(b);}}}
function cs(f,h){yr();for(var e in f){if(e==parseInt(e)){var a=f[e];for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.w();if(hs(h,d)){return true;}}}}return false;}
function ds(a){return zr(this,a);}
function es(c,d){yr();for(var b in c){if(b.charCodeAt(0)==58){var a=c[b];if(hs(d,a)){return true;}}}return false;}
function fs(){yr();}
function gs(){return Br(this);}
function hs(a,b){yr();if(a===b){return true;}else if(a===null){return false;}else{return a.eQ(b);}}
function ks(a){return Cr(this,a);}
function is(f,h,e){yr();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.v();if(hs(h,d)){return c.w();}}}}
function js(b,a){yr();return b[':'+a];}
function ls(f,h,e){yr();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.v();if(hs(h,d)){if(a.length==1){delete f[e];}else{a.splice(g,1);}return c.w();}}}}
function ms(c,a){yr();a=':'+a;var b=c[a];delete c[a];return b;}
function Dq(){}
_=Dq.prototype=new Bo();_.p=ds;_.u=gs;_.y=ks;_.tI=42;_.a=null;_.b=null;_.c=0;_.d=null;var Fr;function Fq(b,a,c){b.a=a;b.b=c;return b;}
function br(a,b){return Fq(new Eq(),a,b);}
function cr(b){var a;if(mb(b,14)){a=lb(b,14);if(hs(this.a,a.v())&&hs(this.b,a.w())){return true;}}return false;}
function dr(){return this.a;}
function er(){return this.b;}
function fr(){var a,b;a=0;b=0;if(this.a!==null){a=this.a.hC();}if(this.b!==null){b=this.b.hC();}return a^b;}
function Eq(){}
_=Eq.prototype=new en();_.eQ=cr;_.v=dr;_.w=er;_.hC=fr;_.tI=43;_.a=null;_.b=null;function pr(b,a){b.a=a;return b;}
function rr(a){return ir(new hr(),a.a);}
function sr(c){var a,b,d;if(mb(c,14)){a=lb(c,14);b=a.v();if(zr(this.a,b)){d=Cr(this.a,b);return hs(a.w(),d);}}return false;}
function tr(){return rr(this);}
function ur(){return this.a.c;}
function gr(){}
_=gr.prototype=new bq();_.q=sr;_.B=tr;_.lb=ur;_.tI=44;function ir(c,b){var a;c.c=b;a=iq(new gq());if(c.c.b!==(yr(),Fr)){jq(a,Fq(new Eq(),null,c.c.b));}bs(c.c.d,a);as(c.c.a,a);c.a=uo(a);return c;}
function kr(a){return no(a.a);}
function lr(a){return a.b=lb(oo(a.a),14);}
function mr(a){if(a.b===null){throw Dm(new Cm(),'Must call next() before remove().');}else{po(a.a);Er(a.c,a.b.v());a.b=null;}}
function nr(){return kr(this);}
function or(){return lr(this);}
function hr(){}
_=hr.prototype=new en();_.z=nr;_.D=or;_.tI=0;_.a=null;_.b=null;function rs(){}
_=rs.prototype=new jn();_.tI=45;function xs(a){a.e=hk(new ck());a.c=bj(new aj());a.b=mj(new kj(),'interfaceRadioGroup','MOD Curator');a.d=mj(new kj(),'interfaceRadioGroup','Reference Genome');a.a=fj(new dj(),'Login','Login');}
function ys(a){xs(a);return a;}
function As(a){var b;b=Ak(new yk());Bk(b,Fh(new Dh(),'<h2>Login<\/h2>'));Bk(b,a.e);Bk(b,a.c);Bk(b,a.d);Bk(b,a.b);Bk(b,a.a);return b;}
function ws(){}
_=ws.prototype=new en();_.tI=0;function Ds(b){var a;a=ys(new ws());ff(wj(),As(a));}
function Bs(){}
_=Bs.prototype=new en();_.tI=0;function om(){Ds(new Bs());}
function gwtOnLoad(b,d,c){$moduleName=d;$moduleBase=c;if(b)try{om();}catch(a){b(d);}else{om();}}
var qb=[{},{},{1:1},{3:1},{3:1},{3:1},{3:1},{2:1},{2:1,4:1},{2:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{6:1,7:1,8:1,9:1,10:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{11:1},{12:1},{13:1},{13:1},{11:1},{12:1},{14:1},{13:1},{3:1}];if (org_bbop_RefGenome) {  var __gwt_initHandlers = org_bbop_RefGenome.__gwt_initHandlers;  org_bbop_RefGenome.onScriptLoad(gwtOnLoad);}})();