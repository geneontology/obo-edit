(function(){var $wnd = window;var $doc = $wnd.document;var $moduleName, $moduleBase;var _,vs='com.google.gwt.core.client.',ws='com.google.gwt.lang.',xs='com.google.gwt.user.client.',ys='com.google.gwt.user.client.impl.',zs='com.google.gwt.user.client.ui.',As='com.google.gwt.user.client.ui.impl.',Bs='java.lang.',Cs='java.util.',Ds='org.bbop.client.',Es='org.bbop.client.Pages.RefGenome.';function ls(){}
function Cm(a){return this===a;}
function Dm(){return sn(this);}
function Am(){}
_=Am.prototype={};_.eQ=Cm;_.hC=Dm;_.tI=1;var o=null;function r(a){return a==null?0:a.$H?a.$H:(a.$H=t());}
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
_=v.prototype=new Am();_.eQ=C;_.hC=D;_.tI=7;function F(c,a,d,b,e){c.a=a;c.b=b;e;c.tI=d;return c;}
function bb(a,b,c){return a[b]=c;}
function cb(b,a){return b[a];}
function db(a){return a.length;}
function fb(e,d,c,b,a){return eb(e,d,c,b,0,db(b),a);}
function eb(j,i,g,c,e,a,b){var d,f,h;if((f=cb(c,e))<0){throw new ym();}h=F(new E(),f,cb(i,e),cb(g,e),j);++e;if(e<a){j=jn(j,1);for(d=0;d<f;++d){bb(h,d,eb(j,i,g,c,e,a,b));}}else{for(d=0;d<f;++d){bb(h,d,b);}}return h;}
function gb(a,b,c){if(c!==null&&a.b!=0&& !mb(c,a.b)){throw new fm();}return bb(a,b,c);}
function E(){}
_=E.prototype=new Am();_.tI=0;function jb(b,a){return !(!(b&&qb[b][a]));}
function kb(a){return String.fromCharCode(a);}
function lb(b,a){if(b!=null)jb(b.tI,a)||pb();return b;}
function mb(b,a){return b!=null&&jb(b.tI,a);}
function nb(a){return a&65535;}
function pb(){throw new im();}
function ob(a){if(a!==null){throw new im();}return a;}
function rb(b,d){_=d.prototype;if(b&& !(b.tI>=_.tI)){var c=b.toString;for(var a in _){b[a]=_[a];}b.toString=c;}return b;}
var qb;function vb(){vb=ls;vc=Ep(new Cp());{pc=new Dd();be(pc);}}
function wb(b,a){vb();qe(pc,b,a);}
function xb(a,b){vb();return Fd(pc,a,b);}
function yb(){vb();return se(pc,'div');}
function zb(){vb();return te(pc,'password');}
function Ab(a){vb();return ie(pc,a);}
function Bb(){vb();return te(pc,'text');}
function Cb(){vb();return se(pc,'label');}
function Db(){vb();return se(pc,'span');}
function Eb(){vb();return se(pc,'tbody');}
function Fb(){vb();return se(pc,'td');}
function ac(){vb();return se(pc,'tr');}
function bc(){vb();return se(pc,'table');}
function ec(b,a,d){vb();var c;c=o;{dc(b,a,d);}}
function dc(b,a,c){vb();var d;if(a===uc){if(jc(b)==8192){uc=null;}}d=cc;cc=b;try{c.E(b);}finally{cc=d;}}
function fc(b,a){vb();ue(pc,b,a);}
function gc(a){vb();return ve(pc,a);}
function hc(a){vb();return je(pc,a);}
function ic(a){vb();return ke(pc,a);}
function jc(a){vb();return we(pc,a);}
function kc(a){vb();le(pc,a);}
function mc(a,b){vb();return ye(pc,a,b);}
function lc(a,b){vb();return xe(pc,a,b);}
function nc(a){vb();return ze(pc,a);}
function oc(a){vb();return me(pc,a);}
function qc(b,a){vb();return ce(pc,b,a);}
function rc(a){vb();var b,c;c=true;if(vc.b>0){b=ob(cq(vc,vc.b-1));if(!(c=null.mb())){fc(a,true);kc(a);}}return c;}
function sc(a){vb();if(uc!==null&&xb(a,uc)){uc=null;}de(pc,a);}
function tc(b,a){vb();Ae(pc,b,a);}
function wc(a){vb();uc=a;oe(pc,a);}
function yc(a,b,c){vb();Ce(pc,a,b,c);}
function xc(a,b,c){vb();Be(pc,a,b,c);}
function zc(a,b){vb();De(pc,a,b);}
function Ac(a,b){vb();Ee(pc,a,b);}
function Bc(a,b){vb();Fe(pc,a,b);}
function Cc(b,a,c){vb();af(pc,b,a,c);}
function Dc(a,b){vb();fe(pc,a,b);}
var cc=null,pc=null,uc=null,vc;function ad(a){if(mb(a,4)){return xb(this,lb(a,4));}return x(rb(this,Ec),a);}
function bd(){return y(rb(this,Ec));}
function Ec(){}
_=Ec.prototype=new v();_.eQ=ad;_.hC=bd;_.tI=8;function fd(a){return x(rb(this,cd),a);}
function gd(){return y(rb(this,cd));}
function cd(){}
_=cd.prototype=new v();_.eQ=fd;_.hC=gd;_.tI=9;function md(){md=ls;od=Ep(new Cp());{nd();}}
function nd(){md();sd(new id());}
var od;function kd(){while((md(),od).b>0){ob(cq((md(),od),0)).mb();}}
function ld(){return null;}
function id(){}
_=id.prototype=new Am();_.fb=kd;_.gb=ld;_.tI=10;function rd(){rd=ls;td=Ep(new Cp());Bd=Ep(new Cp());{xd();}}
function sd(a){rd();Fp(td,a);}
function ud(){rd();var a,b;for(a=ko(td);co(a);){b=lb(eo(a),5);b.fb();}}
function vd(){rd();var a,b,c,d;d=null;for(a=ko(td);co(a);){b=lb(eo(a),5);c=b.gb();{d=c;}}return d;}
function wd(){rd();var a,b;for(a=ko(Bd);co(a);){b=ob(eo(a));null.mb();}}
function xd(){rd();__gwt_initHandlers(function(){Ad();},function(){return zd();},function(){yd();$wnd.onresize=null;$wnd.onbeforeclose=null;$wnd.onclose=null;});}
function yd(){rd();var a;a=o;{ud();}}
function zd(){rd();var a;a=o;{return vd();}}
function Ad(){rd();var a;a=o;{wd();}}
var td,Bd;function qe(c,b,a){b.appendChild(a);}
function se(b,a){return $doc.createElement(a);}
function te(b,c){var a=$doc.createElement('INPUT');a.type=c;return a;}
function ue(c,b,a){b.cancelBubble=a;}
function ve(b,a){return a.which||(a.keyCode|| -1);}
function we(b,a){switch(a.type){case 'blur':return 4096;case 'change':return 1024;case 'click':return 1;case 'dblclick':return 2;case 'focus':return 2048;case 'keydown':return 128;case 'keypress':return 256;case 'keyup':return 512;case 'load':return 32768;case 'losecapture':return 8192;case 'mousedown':return 4;case 'mousemove':return 64;case 'mouseout':return 32;case 'mouseover':return 16;case 'mouseup':return 8;case 'scroll':return 16384;case 'error':return 65536;case 'mousewheel':return 131072;case 'DOMMouseScroll':return 131072;}}
function ye(d,a,b){var c=a[b];return c==null?null:String(c);}
function xe(c,a,b){return !(!a[b]);}
function ze(b,a){return a.__eventBits||0;}
function Ae(c,b,a){b.removeChild(a);}
function Ce(c,a,b,d){a[b]=d;}
function Be(c,a,b,d){a[b]=d;}
function De(c,a,b){a.__listener=b;}
function Ee(c,a,b){if(!b){b='';}a.innerHTML=b;}
function Fe(c,a,b){while(a.firstChild){a.removeChild(a.firstChild);}if(b!=null){a.appendChild($doc.createTextNode(b));}}
function af(c,b,a,d){b.style[a]=d;}
function Cd(){}
_=Cd.prototype=new Am();_.tI=0;function ie(c,b){var a=$doc.createElement('INPUT');a.type='radio';a.name=b;return a;}
function je(b,a){return a.target||null;}
function ke(b,a){return a.relatedTarget||null;}
function le(b,a){a.preventDefault();}
function me(c,a){var b=a.parentNode;if(b==null){return null;}if(b.nodeType!=1)b=null;return b||null;}
function ne(d){$wnd.__dispatchCapturedMouseEvent=function(b){if($wnd.__dispatchCapturedEvent(b)){var a=$wnd.__captureElem;if(a&&a.__listener){ec(b,a,a.__listener);b.stopPropagation();}}};$wnd.__dispatchCapturedEvent=function(a){if(!rc(a)){a.stopPropagation();a.preventDefault();return false;}return true;};$wnd.addEventListener('click',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('dblclick',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousedown',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mouseup',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousemove',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousewheel',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('keydown',$wnd.__dispatchCapturedEvent,true);$wnd.addEventListener('keyup',$wnd.__dispatchCapturedEvent,true);$wnd.addEventListener('keypress',$wnd.__dispatchCapturedEvent,true);$wnd.__dispatchEvent=function(b){var c,a=this;while(a&& !(c=a.__listener))a=a.parentNode;if(a&&a.nodeType!=1)a=null;if(c)ec(b,a,c);};$wnd.__captureElem=null;}
function oe(b,a){$wnd.__captureElem=a;}
function pe(c,b,a){b.__eventBits=a;b.onclick=a&1?$wnd.__dispatchEvent:null;b.ondblclick=a&2?$wnd.__dispatchEvent:null;b.onmousedown=a&4?$wnd.__dispatchEvent:null;b.onmouseup=a&8?$wnd.__dispatchEvent:null;b.onmouseover=a&16?$wnd.__dispatchEvent:null;b.onmouseout=a&32?$wnd.__dispatchEvent:null;b.onmousemove=a&64?$wnd.__dispatchEvent:null;b.onkeydown=a&128?$wnd.__dispatchEvent:null;b.onkeypress=a&256?$wnd.__dispatchEvent:null;b.onkeyup=a&512?$wnd.__dispatchEvent:null;b.onchange=a&1024?$wnd.__dispatchEvent:null;b.onfocus=a&2048?$wnd.__dispatchEvent:null;b.onblur=a&4096?$wnd.__dispatchEvent:null;b.onlosecapture=a&8192?$wnd.__dispatchEvent:null;b.onscroll=a&16384?$wnd.__dispatchEvent:null;b.onload=a&32768?$wnd.__dispatchEvent:null;b.onerror=a&65536?$wnd.__dispatchEvent:null;b.onmousewheel=a&131072?$wnd.__dispatchEvent:null;}
function ge(){}
_=ge.prototype=new Cd();_.tI=0;function Fd(c,a,b){if(!a&& !b){return true;}else if(!a|| !b){return false;}return a.isSameNode(b);}
function be(a){ne(a);ae(a);}
function ae(d){$wnd.addEventListener('mouseout',function(b){var a=$wnd.__captureElem;if(a&& !b.relatedTarget){if('html'==b.target.tagName.toLowerCase()){var c=$doc.createEvent('MouseEvents');c.initMouseEvent('mouseup',true,true,$wnd,0,b.screenX,b.screenY,b.clientX,b.clientY,b.ctrlKey,b.altKey,b.shiftKey,b.metaKey,b.button,null);a.dispatchEvent(c);}}},true);$wnd.addEventListener('DOMMouseScroll',$wnd.__dispatchCapturedMouseEvent,true);}
function ce(d,c,b){while(b){if(c.isSameNode(b)){return true;}try{b=b.parentNode;}catch(a){return false;}if(b&&b.nodeType!=1){b=null;}}return false;}
function de(b,a){if(a.isSameNode($wnd.__captureElem)){$wnd.__captureElem=null;}}
function fe(c,b,a){pe(c,b,a);ee(c,b,a);}
function ee(c,b,a){if(a&131072){b.addEventListener('DOMMouseScroll',$wnd.__dispatchEvent,false);}}
function Dd(){}
_=Dd.prototype=new ge();_.tI=0;function ik(b,a){jk(b,lk(b)+kb(45)+a);}
function jk(b,a){vk(b.m,a,true);}
function lk(a){return tk(a.m);}
function mk(b,a){nk(b,lk(b)+kb(45)+a);}
function nk(b,a){vk(b.m,a,false);}
function ok(d,b,a){var c=b.parentNode;if(!c){return;}c.insertBefore(a,b);c.removeChild(b);}
function pk(b,a){if(b.m!==null){ok(b,b.m,a);}b.m=a;}
function qk(b,a){uk(b.m,a);}
function rk(b,a){Dc(b.m,a|nc(b.m));}
function sk(a){return mc(a,'className');}
function tk(a){var b,c;b=sk(a);c=en(b,32);if(c>=0){return kn(b,0,c);}return b;}
function uk(a,b){yc(a,'className',b);}
function vk(c,j,a){var b,d,e,f,g,h,i;if(c===null){throw Fm(new Em(),'Null widget handle. If you are creating a composite, ensure that initWidget() has been called.');}j=ln(j);if(hn(j)==0){throw qm(new pm(),'Style names cannot be empty');}i=sk(c);e=fn(i,j);while(e!=(-1)){if(e==0||cn(i,e-1)==32){f=e+hn(j);g=hn(i);if(f==g||f<g&&cn(i,f)==32){break;}}e=gn(i,j,e+1);}if(a){if(e==(-1)){if(hn(i)>0){i+=' ';}yc(c,'className',i+j);}}else{if(e!=(-1)){b=ln(kn(i,0,e));d=ln(jn(i,e+hn(j)));if(hn(b)==0){h=d;}else if(hn(d)==0){h=b;}else{h=b+' '+d;}yc(c,'className',h);}}}
function hk(){}
_=hk.prototype=new Am();_.tI=0;_.m=null;function pl(a){if(a.k){throw tm(new sm(),"Should only call onAttach when the widget is detached from the browser's document");}a.k=true;zc(a.m,a);a.r();a.db();}
function ql(a){if(!a.k){throw tm(new sm(),"Should only call onDetach when the widget is attached to the browser's document");}try{a.eb();}finally{a.s();zc(a.m,null);a.k=false;}}
function rl(a){if(a.l!==null){a.l.ib(a);}else if(a.l!==null){throw tm(new sm(),"This widget's parent does not implement HasWidgets");}}
function sl(b,a){if(b.k){zc(b.m,null);}pk(b,a);if(b.k){zc(a,b);}}
function tl(c,b){var a;a=c.l;if(b===null){if(a!==null&&a.k){c.cb();}c.l=null;}else{if(a!==null){throw tm(new sm(),'Cannot set a new parent without first clearing the old parent');}c.l=b;if(b.k){c.D();}}}
function ul(){}
function vl(){}
function wl(){pl(this);}
function xl(a){}
function yl(){ql(this);}
function zl(){}
function Al(){}
function Bl(a){sl(this,a);}
function Dk(){}
_=Dk.prototype=new hk();_.r=ul;_.s=vl;_.D=wl;_.E=xl;_.cb=yl;_.db=zl;_.eb=Al;_.jb=Bl;_.tI=11;_.k=false;_.l=null;function xi(b,a){tl(a,b);}
function zi(b,a){tl(a,null);}
function Ai(){var a,b;for(b=this.A();cl(b);){a=dl(b);a.D();}}
function Bi(){var a,b;for(b=this.A();cl(b);){a=dl(b);a.cb();}}
function Ci(){}
function Di(){}
function wi(){}
_=wi.prototype=new Dk();_.r=Ai;_.s=Bi;_.db=Ci;_.eb=Di;_.tI=12;function Bf(a){a.e=gl(new Ek(),a);}
function Cf(a){Bf(a);return a;}
function Df(c,a,b){rl(a);hl(c.e,a);wb(b,a.m);xi(c,a);}
function Ff(b,c){var a;if(c.l!==b){return false;}zi(b,c);a=c.m;tc(oc(a),a);nl(b.e,c);return true;}
function ag(){return ll(this.e);}
function bg(a){return Ff(this,a);}
function Af(){}
_=Af.prototype=new wi();_.A=ag;_.ib=bg;_.tI=13;function cf(a){Cf(a);a.jb(yb());Cc(a.m,'position','relative');Cc(a.m,'overflow','hidden');return a;}
function df(a,b){Df(a,b,a.m);}
function ff(a){Cc(a,'left','');Cc(a,'top','');Cc(a,'position','');}
function gf(b){var a;a=Ff(this,b);if(a){ff(b.m);}return a;}
function bf(){}
_=bf.prototype=new Af();_.ib=gf;_.tI=14;function vh(){vh=ls;Fl(),dm;}
function uh(b,a){Fl(),dm;xh(b,a);return b;}
function wh(b,a){switch(jc(a)){case 1:break;case 4096:case 2048:break;case 128:case 512:case 256:break;}}
function xh(b,a){sl(b,a);rk(b,7041);}
function yh(){return !lc(this.m,'disabled');}
function zh(a){wh(this,a);}
function Ah(a){xh(this,a);}
function th(){}
_=th.prototype=new Dk();_.z=yh;_.E=zh;_.jb=Ah;_.tI=15;function kf(){kf=ls;Fl(),dm;}
function jf(b,a){Fl(),dm;uh(b,a);return b;}
function hf(){}
_=hf.prototype=new th();_.tI=16;function mf(a){Cf(a);a.d=bc();a.c=Eb();wb(a.d,a.c);a.jb(a.d);return a;}
function of(c,b,a){yc(b,'align',a.a);}
function pf(c,b,a){Cc(b,'verticalAlign',a.a);}
function lf(){}
_=lf.prototype=new Af();_.tI=17;_.c=null;_.d=null;function sf(){sf=ls;Fl(),dm;}
function rf(b,a){var c;Fl(),dm;jf(b,Db());b.a=a;b.b=Cb();Dc(b.a,nc(b.m));Dc(b.m,0);wb(b.m,b.a);wb(b.m,b.b);c='check'+ ++zf;yc(b.a,'id',c);yc(b.b,'htmlFor',c);return b;}
function tf(b){var a;a=b.k?'checked':'defaultChecked';return lc(b.a,a);}
function uf(b,a){xc(b.a,'checked',a);xc(b.a,'defaultChecked',a);}
function vf(b,a){Bc(b.b,a);}
function wf(){return !lc(this.a,'disabled');}
function xf(){zc(this.a,this);}
function yf(){zc(this.a,null);uf(this,tf(this));}
function qf(){}
_=qf.prototype=new hf();_.z=wf;_.db=xf;_.eb=yf;_.tI=18;_.a=null;_.b=null;var zf=0;function qg(){qg=ls;Fl(),dm;}
function ng(a,b){Fl(),dm;mg(a);kg(a.h,b);return a;}
function og(b,c,a){Fl(),dm;ng(b,c);kg(vg(b),a);return b;}
function mg(a){Fl(),dm;jf(a,am((rh(),sh)));rk(a,6269);gh(a,rg(a,null,'up',0));qk(a,'gwt-CustomButton');return a;}
function pg(a){if(a.f||a.g){sc(a.m);a.f=false;a.g=false;a.F();}}
function rg(d,a,c,b){return eg(new dg(),a,d,c,b);}
function sg(a){if(a.a===null){Eg(a,a.h);}}
function tg(a){sg(a);return a.a;}
function ug(a){if(a.d===null){Fg(a,rg(a,vg(a),'down-disabled',5));}return a.d;}
function vg(a){if(a.c===null){ah(a,rg(a,a.h,'down',1));}return a.c;}
function wg(a){if(a.e===null){bh(a,rg(a,vg(a),'down-hovering',3));}return a.e;}
function xg(b,a){switch(a){case 1:return vg(b);case 0:return b.h;case 3:return wg(b);case 2:return zg(b);case 4:return yg(b);case 5:return ug(b);default:throw tm(new sm(),a+' is not a known face id.');}}
function yg(a){if(a.i===null){fh(a,rg(a,a.h,'up-disabled',4));}return a.i;}
function zg(a){if(a.j===null){hh(a,rg(a,a.h,'up-hovering',2));}return a.j;}
function Ag(a){return (1&tg(a).a)>0;}
function Bg(a){return (2&tg(a).a)>0;}
function Eg(b,a){if(b.a!==a){if(b.a!==null){mk(b,b.a.b);}b.a=a;Cg(b,jg(a));ik(b,b.a.b);}}
function Dg(c,a){var b;b=xg(c,a);Eg(c,b);}
function Cg(b,a){if(b.b!==a){if(b.b!==null){tc(b.m,b.b);}b.b=a;wb(b.m,b.b);}}
function ch(b,a){if(a!=Ag(b)){ih(b);}}
function Fg(b,a){b.d=a;}
function ah(b,a){b.c=a;}
function bh(b,a){b.e=a;}
function dh(b,a){if(a){bm((rh(),sh),b.m);}else{El((rh(),sh),b.m);}}
function eh(b,a){if(a!=Bg(b)){jh(b);}}
function fh(a,b){a.i=b;}
function gh(a,b){a.h=b;}
function hh(a,b){a.j=b;}
function ih(b){var a;a=tg(b).a^1;Dg(b,a);}
function jh(b){var a;a=tg(b).a^2;a&=(-5);Dg(b,a);}
function kh(){sg(this);pl(this);}
function lh(a){var b,c;if(this.z()==false){return;}c=jc(a);switch(c){case 4:dh(this,true);this.ab();wc(this.m);this.f=true;kc(a);break;case 8:if(this.f){this.f=false;sc(this.m);if(Bg(this)){this.bb();}}break;case 64:if(this.f){kc(a);}break;case 32:if(qc(this.m,hc(a))&& !qc(this.m,ic(a))){if(this.f){this.F();}eh(this,false);}break;case 16:if(qc(this.m,hc(a))){eh(this,true);if(this.f){this.ab();}}break;case 1:return;case 4096:if(this.g){this.g=false;this.F();}break;case 8192:if(this.f){this.f=false;this.F();}break;}wh(this,a);b=nb(gc(a));switch(c){case 128:if(b==32){this.g=true;this.ab();}break;case 512:if(this.g&&b==32){this.g=false;this.bb();}break;case 256:if(b==10||b==13){this.ab();this.bb();}break;}}
function oh(){}
function mh(){}
function nh(){}
function ph(){ql(this);pg(this);}
function cg(){}
_=cg.prototype=new hf();_.D=kh;_.E=lh;_.bb=oh;_.F=mh;_.ab=nh;_.cb=ph;_.tI=19;_.a=null;_.b=null;_.c=null;_.d=null;_.e=null;_.f=false;_.g=false;_.h=null;_.i=null;_.j=null;function hg(c,a,b){c.e=b;c.c=a;return c;}
function jg(a){if(a.d===null){if(a.c===null){a.d=yb();return a.d;}else{return jg(a.c);}}else{return a.d;}}
function kg(b,a){b.d=yb();vk(b.d,'html-face',true);Bc(b.d,a);lg(b);}
function lg(a){if(a.e.a!==null&&jg(a.e.a)===jg(a)){Cg(a.e,a.d);}}
function gg(){}
_=gg.prototype=new Am();_.tI=0;_.c=null;_.d=null;function eg(c,a,b,e,d){c.b=e;c.a=d;hg(c,a,b);return c;}
function dg(){}
_=dg.prototype=new gg();_.tI=0;function rh(){rh=ls;sh=(Fl(),cm);}
var sh;function ti(a){a.jb(yb());rk(a,131197);qk(a,'gwt-Label');return a;}
function vi(a){switch(jc(a)){case 1:break;case 4:case 8:case 64:case 16:case 32:break;case 131072:break;}}
function si(){}
_=si.prototype=new Dk();_.E=vi;_.tI=20;function Ch(a){ti(a);a.jb(yb());rk(a,125);qk(a,'gwt-HTML');return a;}
function Dh(b,a){Ch(b);Fh(b,a);return b;}
function Fh(b,a){Ac(b.m,a);}
function Bh(){}
_=Bh.prototype=new si();_.tI=21;function gi(){gi=ls;ei(new di(),'center');hi=ei(new di(),'left');ei(new di(),'right');}
var hi;function ei(b,a){b.a=a;return b;}
function di(){}
_=di.prototype=new Am();_.tI=0;_.a=null;function ni(){ni=ls;li(new ki(),'bottom');li(new ki(),'middle');oi=li(new ki(),'top');}
var oi;function li(a,b){a.a=b;return a;}
function ki(){}
_=ki.prototype=new Am();_.tI=0;_.a=null;function dk(){dk=ls;Fl(),dm;}
function ck(b,a){Fl(),dm;uh(b,a);rk(b,1024);return b;}
function ek(a){var b;wh(this,a);b=jc(a);}
function bk(){}
_=bk.prototype=new th();_.E=ek;_.tI=22;function aj(){aj=ls;Fl(),dm;}
function Fi(a){Fl(),dm;ck(a,zb());qk(a,'gwt-PasswordTextBox');return a;}
function Ei(){}
_=Ei.prototype=new bk();_.tI=23;function ej(){ej=ls;Fl(),dm;}
function cj(a){{qk(a,'gwt-PushButton');}}
function dj(b,c,a){Fl(),dm;og(b,c,a);cj(b);return b;}
function hj(){ch(this,false);}
function fj(){ch(this,false);}
function gj(){ch(this,true);}
function bj(){}
_=bj.prototype=new cg();_.bb=hj;_.F=fj;_.ab=gj;_.tI=24;function lj(){lj=ls;Fl(),dm;}
function jj(b,a){Fl(),dm;rf(b,Ab(a));qk(b,'gwt-RadioButton');return b;}
function kj(c,b,a){Fl(),dm;jj(c,b);vf(c,a);return c;}
function ij(){}
_=ij.prototype=new qf();_.tI=25;function sj(){sj=ls;xj=mr(new tq());}
function rj(b,a){sj();cf(b);if(a===null){a=tj();}b.jb(a);b.D();return b;}
function uj(){sj();return vj(null);}
function vj(c){sj();var a,b;b=lb(sr(xj,c),6);if(b!==null){return b;}a=null;if(xj.c==0){wj();}tr(xj,c,b=rj(new mj(),a));return b;}
function tj(){sj();return $doc.body;}
function wj(){sj();sd(new nj());}
function mj(){}
_=mj.prototype=new bf();_.tI=26;var xj;function pj(){var a,b;for(b=dp(rp((sj(),xj)));kp(b);){a=lb(lp(b),6);if(a.k){a.cb();}}}
function qj(){return null;}
function nj(){}
_=nj.prototype=new Am();_.fb=pj;_.gb=qj;_.tI=27;function gk(){gk=ls;Fl(),dm;}
function fk(a){Fl(),dm;ck(a,Bb());qk(a,'gwt-TextBox');return a;}
function ak(){}
_=ak.prototype=new bk();_.tI=28;function xk(a){a.a=(gi(),hi);a.b=(ni(),oi);}
function yk(a){mf(a);xk(a);yc(a.d,'cellSpacing','0');yc(a.d,'cellPadding','0');return a;}
function zk(b,d){var a,c;c=ac();a=Bk(b);wb(c,a);wb(b.c,c);Df(b,d,a);}
function Bk(b){var a;a=Fb();of(b,a,b.a);pf(b,a,b.b);return a;}
function Ck(c){var a,b;b=oc(c.m);a=Ff(this,c);if(a){tc(this.c,oc(b));}return a;}
function wk(){}
_=wk.prototype=new lf();_.ib=Ck;_.tI=29;function gl(b,a){b.a=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[4],null);return b;}
function hl(a,b){kl(a,b,a.b);}
function jl(b,c){var a;for(a=0;a<b.b;++a){if(b.a[a]===c){return a;}}return (-1);}
function kl(d,e,a){var b,c;if(a<0||a>d.b){throw new vm();}if(d.b==d.a.a){c=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[d.a.a*2],null);for(b=0;b<d.a.a;++b){gb(c,b,d.a[b]);}d.a=c;}++d.b;for(b=d.b-1;b>a;--b){gb(d.a,b,d.a[b-1]);}gb(d.a,a,e);}
function ll(a){return al(new Fk(),a);}
function ml(c,b){var a;if(b<0||b>=c.b){throw new vm();}--c.b;for(a=b;a<c.b;++a){gb(c.a,a,c.a[a+1]);}gb(c.a,c.b,null);}
function nl(b,c){var a;a=jl(b,c);if(a==(-1)){throw new hs();}ml(b,a);}
function Ek(){}
_=Ek.prototype=new Am();_.tI=0;_.a=null;_.b=0;function al(b,a){b.b=a;return b;}
function cl(a){return a.a<a.b.b-1;}
function dl(a){if(a.a>=a.b.b){throw new hs();}return a.b.a[++a.a];}
function el(){return cl(this);}
function fl(){return dl(this);}
function Fk(){}
_=Fk.prototype=new Am();_.y=el;_.C=fl;_.tI=0;_.a=(-1);function Fl(){Fl=ls;cm=Dl(new Cl());dm=cm;}
function Dl(a){Fl();return a;}
function El(b,a){a.blur();}
function am(b){var a=$doc.createElement('DIV');a.tabIndex=0;return a;}
function bm(b,a){a.focus();}
function Cl(){}
_=Cl.prototype=new Am();_.tI=0;var cm,dm;function un(b,a){a;return b;}
function tn(){}
_=tn.prototype=new Am();_.tI=3;function nm(b,a){un(b,a);return b;}
function mm(){}
_=mm.prototype=new tn();_.tI=4;function Fm(b,a){nm(b,a);return b;}
function Em(){}
_=Em.prototype=new mm();_.tI=5;function fm(){}
_=fm.prototype=new Em();_.tI=30;function im(){}
_=im.prototype=new Em();_.tI=31;function qm(b,a){Fm(b,a);return b;}
function pm(){}
_=pm.prototype=new Em();_.tI=32;function tm(b,a){Fm(b,a);return b;}
function sm(){}
_=sm.prototype=new Em();_.tI=33;function wm(b,a){Fm(b,a);return b;}
function vm(){}
_=vm.prototype=new Em();_.tI=34;function ym(){}
_=ym.prototype=new Em();_.tI=35;function cn(b,a){return b.charCodeAt(a);}
function en(b,a){return b.indexOf(String.fromCharCode(a));}
function fn(b,a){return b.indexOf(a);}
function gn(c,b,a){return c.indexOf(b,a);}
function hn(a){return a.length;}
function jn(b,a){return b.substr(a,b.length-a);}
function kn(c,a,b){return c.substr(a,b-a);}
function ln(c){var a=c.replace(/^(\s*)/,'');var b=a.replace(/\s*$/,'');return b;}
function mn(a,b){return String(a)==b;}
function nn(a){if(!mb(a,1))return false;return mn(this,a);}
function pn(){var a=on;if(!a){a=on={};}var e=':'+this;var b=a[e];if(b==null){b=0;var f=this.length;var d=f<64?1:f/32|0;for(var c=0;c<f;c+=d){b<<=1;b+=this.charCodeAt(c);}b|=0;a[e]=b;}return b;}
_=String.prototype;_.eQ=nn;_.hC=pn;_.tI=2;var on=null;function sn(a){return s(a);}
function xn(b,a){Fm(b,a);return b;}
function wn(){}
_=wn.prototype=new Em();_.tI=36;function An(d,a,b){var c;while(a.y()){c=a.C();if(b===null?c===null:b.eQ(c)){return a;}}return null;}
function Cn(a){throw xn(new wn(),'add');}
function Dn(b){var a;a=An(this,this.A(),b);return a!==null;}
function zn(){}
_=zn.prototype=new Am();_.o=Cn;_.q=Dn;_.tI=0;function jo(b,a){throw wm(new vm(),'Index: '+a+', Size: '+b.b);}
function ko(a){return ao(new Fn(),a);}
function lo(b,a){throw xn(new wn(),'add');}
function mo(a){this.n(this.kb(),a);return true;}
function no(e){var a,b,c,d,f;if(e===this){return true;}if(!mb(e,11)){return false;}f=lb(e,11);if(this.kb()!=f.kb()){return false;}c=ko(this);d=f.A();while(co(c)){a=eo(c);b=eo(d);if(!(a===null?b===null:a.eQ(b))){return false;}}return true;}
function oo(){var a,b,c,d;c=1;a=31;b=ko(this);while(co(b)){d=eo(b);c=31*c+(d===null?0:d.hC());}return c;}
function po(){return ko(this);}
function qo(a){throw xn(new wn(),'remove');}
function En(){}
_=En.prototype=new zn();_.n=lo;_.o=mo;_.eQ=no;_.hC=oo;_.A=po;_.hb=qo;_.tI=37;function ao(b,a){b.c=a;return b;}
function co(a){return a.a<a.c.kb();}
function eo(a){if(!co(a)){throw new hs();}return a.c.w(a.b=a.a++);}
function fo(a){if(a.b<0){throw new sm();}a.c.hb(a.b);a.a=a.b;a.b=(-1);}
function go(){return co(this);}
function ho(){return eo(this);}
function Fn(){}
_=Fn.prototype=new Am();_.y=go;_.C=ho;_.tI=0;_.a=0;_.b=(-1);function pp(f,d,e){var a,b,c;for(b=hr(f.t());ar(b);){a=br(b);c=a.u();if(d===null?c===null:d.eQ(c)){if(e){cr(b);}return a;}}return null;}
function qp(b){var a;a=b.t();return to(new so(),b,a);}
function rp(b){var a;a=rr(b);return bp(new ap(),b,a);}
function sp(a){return pp(this,a,false)!==null;}
function tp(d){var a,b,c,e,f,g,h;if(d===this){return true;}if(!mb(d,12)){return false;}f=lb(d,12);c=qp(this);e=f.B();if(!zp(c,e)){return false;}for(a=vo(c);Co(a);){b=Do(a);h=this.x(b);g=f.x(b);if(h===null?g!==null:!h.eQ(g)){return false;}}return true;}
function up(b){var a;a=pp(this,b,false);return a===null?null:a.v();}
function vp(){var a,b,c;b=0;for(c=hr(this.t());ar(c);){a=br(c);b+=a.hC();}return b;}
function wp(){return qp(this);}
function ro(){}
_=ro.prototype=new Am();_.p=sp;_.eQ=tp;_.x=up;_.hC=vp;_.B=wp;_.tI=38;function zp(e,b){var a,c,d;if(b===e){return true;}if(!mb(b,13)){return false;}c=lb(b,13);if(c.kb()!=e.kb()){return false;}for(a=c.A();a.y();){d=a.C();if(!e.q(d)){return false;}}return true;}
function Ap(a){return zp(this,a);}
function Bp(){var a,b,c;a=0;for(b=this.A();b.y();){c=b.C();if(c!==null){a+=c.hC();}}return a;}
function xp(){}
_=xp.prototype=new zn();_.eQ=Ap;_.hC=Bp;_.tI=39;function to(b,a,c){b.a=a;b.b=c;return b;}
function vo(b){var a;a=hr(b.b);return Ao(new zo(),b,a);}
function wo(a){return this.a.p(a);}
function xo(){return vo(this);}
function yo(){return this.b.a.c;}
function so(){}
_=so.prototype=new xp();_.q=wo;_.A=xo;_.kb=yo;_.tI=40;function Ao(b,a,c){b.a=c;return b;}
function Co(a){return a.a.y();}
function Do(b){var a;a=b.a.C();return a.u();}
function Eo(){return Co(this);}
function Fo(){return Do(this);}
function zo(){}
_=zo.prototype=new Am();_.y=Eo;_.C=Fo;_.tI=0;function bp(b,a,c){b.a=a;b.b=c;return b;}
function dp(b){var a;a=hr(b.b);return ip(new hp(),b,a);}
function ep(a){return qr(this.a,a);}
function fp(){return dp(this);}
function gp(){return this.b.a.c;}
function ap(){}
_=ap.prototype=new zn();_.q=ep;_.A=fp;_.kb=gp;_.tI=0;function ip(b,a,c){b.a=c;return b;}
function kp(a){return a.a.y();}
function lp(a){var b;b=a.a.C().v();return b;}
function mp(){return kp(this);}
function np(){return lp(this);}
function hp(){}
_=hp.prototype=new Am();_.y=mp;_.C=np;_.tI=0;function Dp(a){{aq(a);}}
function Ep(a){Dp(a);return a;}
function Fp(b,a){pq(b.a,b.b++,a);return true;}
function aq(a){a.a=z();a.b=0;}
function cq(b,a){if(a<0||a>=b.b){jo(b,a);}return lq(b.a,a);}
function dq(b,a){return eq(b,a,0);}
function eq(c,b,a){if(a<0){jo(c,a);}for(;a<c.b;++a){if(kq(b,lq(c.a,a))){return a;}}return (-1);}
function fq(c,a){var b;b=cq(c,a);nq(c.a,a,1);--c.b;return b;}
function hq(a,b){if(a<0||a>this.b){jo(this,a);}gq(this.a,a,b);++this.b;}
function iq(a){return Fp(this,a);}
function gq(a,b,c){a.splice(b,0,c);}
function jq(a){return dq(this,a)!=(-1);}
function kq(a,b){return a===b||a!==null&&a.eQ(b);}
function mq(a){return cq(this,a);}
function lq(a,b){return a[b];}
function oq(a){return fq(this,a);}
function nq(a,c,b){a.splice(c,b);}
function pq(a,b,c){a[b]=c;}
function qq(){return this.b;}
function Cp(){}
_=Cp.prototype=new En();_.n=hq;_.o=iq;_.q=jq;_.w=mq;_.hb=oq;_.kb=qq;_.tI=41;_.a=null;_.b=0;function or(){or=ls;vr=Br();}
function lr(a){{nr(a);}}
function mr(a){or();lr(a);return a;}
function nr(a){a.a=z();a.d=A();a.b=rb(vr,v);a.c=0;}
function pr(b,a){if(mb(a,1)){return Fr(b.d,lb(a,1))!==vr;}else if(a===null){return b.b!==vr;}else{return Er(b.a,a,a.hC())!==vr;}}
function qr(a,b){if(a.b!==vr&&Dr(a.b,b)){return true;}else if(Ar(a.d,b)){return true;}else if(yr(a.a,b)){return true;}return false;}
function rr(a){return fr(new Cq(),a);}
function sr(c,a){var b;if(mb(a,1)){b=Fr(c.d,lb(a,1));}else if(a===null){b=c.b;}else{b=Er(c.a,a,a.hC());}return b===vr?null:b;}
function tr(c,a,d){var b;{b=c.b;c.b=d;}if(b===vr){++c.c;return null;}else{return b;}}
function ur(c,a){var b;if(mb(a,1)){b=cs(c.d,lb(a,1));}else if(a===null){b=c.b;c.b=rb(vr,v);}else{b=bs(c.a,a,a.hC());}if(b===vr){return null;}else{--c.c;return b;}}
function wr(e,c){or();for(var d in e){if(d==parseInt(d)){var a=e[d];for(var f=0,b=a.length;f<b;++f){c.o(a[f]);}}}}
function xr(d,a){or();for(var c in d){if(c.charCodeAt(0)==58){var e=d[c];var b=xq(c.substring(1),e);a.o(b);}}}
function yr(f,h){or();for(var e in f){if(e==parseInt(e)){var a=f[e];for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.v();if(Dr(h,d)){return true;}}}}return false;}
function zr(a){return pr(this,a);}
function Ar(c,d){or();for(var b in c){if(b.charCodeAt(0)==58){var a=c[b];if(Dr(d,a)){return true;}}}return false;}
function Br(){or();}
function Cr(){return rr(this);}
function Dr(a,b){or();if(a===b){return true;}else if(a===null){return false;}else{return a.eQ(b);}}
function as(a){return sr(this,a);}
function Er(f,h,e){or();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.u();if(Dr(h,d)){return c.v();}}}}
function Fr(b,a){or();return b[':'+a];}
function bs(f,h,e){or();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.u();if(Dr(h,d)){if(a.length==1){delete f[e];}else{a.splice(g,1);}return c.v();}}}}
function cs(c,a){or();a=':'+a;var b=c[a];delete c[a];return b;}
function tq(){}
_=tq.prototype=new ro();_.p=zr;_.t=Cr;_.x=as;_.tI=42;_.a=null;_.b=null;_.c=0;_.d=null;var vr;function vq(b,a,c){b.a=a;b.b=c;return b;}
function xq(a,b){return vq(new uq(),a,b);}
function yq(b){var a;if(mb(b,14)){a=lb(b,14);if(Dr(this.a,a.u())&&Dr(this.b,a.v())){return true;}}return false;}
function zq(){return this.a;}
function Aq(){return this.b;}
function Bq(){var a,b;a=0;b=0;if(this.a!==null){a=this.a.hC();}if(this.b!==null){b=this.b.hC();}return a^b;}
function uq(){}
_=uq.prototype=new Am();_.eQ=yq;_.u=zq;_.v=Aq;_.hC=Bq;_.tI=43;_.a=null;_.b=null;function fr(b,a){b.a=a;return b;}
function hr(a){return Eq(new Dq(),a.a);}
function ir(c){var a,b,d;if(mb(c,14)){a=lb(c,14);b=a.u();if(pr(this.a,b)){d=sr(this.a,b);return Dr(a.v(),d);}}return false;}
function jr(){return hr(this);}
function kr(){return this.a.c;}
function Cq(){}
_=Cq.prototype=new xp();_.q=ir;_.A=jr;_.kb=kr;_.tI=44;function Eq(c,b){var a;c.c=b;a=Ep(new Cp());if(c.c.b!==(or(),vr)){Fp(a,vq(new uq(),null,c.c.b));}xr(c.c.d,a);wr(c.c.a,a);c.a=ko(a);return c;}
function ar(a){return co(a.a);}
function br(a){return a.b=lb(eo(a.a),14);}
function cr(a){if(a.b===null){throw tm(new sm(),'Must call next() before remove().');}else{fo(a.a);ur(a.c,a.b.u());a.b=null;}}
function dr(){return ar(this);}
function er(){return br(this);}
function Dq(){}
_=Dq.prototype=new Am();_.y=dr;_.C=er;_.tI=0;_.a=null;_.b=null;function hs(){}
_=hs.prototype=new Em();_.tI=45;function ns(a){a.e=fk(new ak());a.c=Fi(new Ei());a.b=kj(new ij(),'interfaceRadioGroup','MOD Curator');a.d=kj(new ij(),'interfaceRadioGroup','Reference Genome');a.a=dj(new bj(),'Login','Login');}
function os(a){ns(a);return a;}
function qs(a){var b;b=yk(new wk());zk(b,Dh(new Bh(),'<h2>Login<\/h2>'));zk(b,a.e);zk(b,a.c);zk(b,a.d);zk(b,a.b);zk(b,a.a);return b;}
function ms(){}
_=ms.prototype=new Am();_.tI=0;function ts(b){var a;a=os(new ms());df(uj(),qs(a));}
function rs(){}
_=rs.prototype=new Am();_.tI=0;function em(){ts(new rs());}
function gwtOnLoad(b,d,c){$moduleName=d;$moduleBase=c;if(b)try{em();}catch(a){b(d);}else{em();}}
var qb=[{},{},{1:1},{3:1},{3:1},{3:1},{3:1},{2:1},{2:1,4:1},{2:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{6:1,7:1,8:1,9:1,10:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{11:1},{12:1},{13:1},{13:1},{11:1},{12:1},{14:1},{13:1},{3:1}];if (org_bbop_RefGenome) {  var __gwt_initHandlers = org_bbop_RefGenome.__gwt_initHandlers;  org_bbop_RefGenome.onScriptLoad(gwtOnLoad);}})();