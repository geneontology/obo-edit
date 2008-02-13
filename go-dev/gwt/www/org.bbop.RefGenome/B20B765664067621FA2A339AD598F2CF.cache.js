(function(){var $wnd = window;var $doc = $wnd.document;var $moduleName, $moduleBase;var _,Ds='com.google.gwt.core.client.',Es='com.google.gwt.lang.',Fs='com.google.gwt.user.client.',at='com.google.gwt.user.client.impl.',bt='com.google.gwt.user.client.ui.',ct='com.google.gwt.user.client.ui.impl.',dt='java.lang.',et='java.util.',ft='org.bbop.client.',gt='org.bbop.client.Pages.RefGenome.';function ts(){}
function en(a){return this===a;}
function fn(){return An(this);}
function cn(){}
_=cn.prototype={};_.eQ=en;_.hC=fn;_.tI=1;var o=null;function r(a){return a==null?0:a.$H?a.$H:(a.$H=t());}
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
_=v.prototype=new cn();_.eQ=C;_.hC=D;_.tI=7;function F(c,a,d,b,e){c.a=a;c.b=b;e;c.tI=d;return c;}
function bb(a,b,c){return a[b]=c;}
function cb(b,a){return b[a];}
function db(a){return a.length;}
function fb(e,d,c,b,a){return eb(e,d,c,b,0,db(b),a);}
function eb(j,i,g,c,e,a,b){var d,f,h;if((f=cb(c,e))<0){throw new an();}h=F(new E(),f,cb(i,e),cb(g,e),j);++e;if(e<a){j=rn(j,1);for(d=0;d<f;++d){bb(h,d,eb(j,i,g,c,e,a,b));}}else{for(d=0;d<f;++d){bb(h,d,b);}}return h;}
function gb(a,b,c){if(c!==null&&a.b!=0&& !mb(c,a.b)){throw new nm();}return bb(a,b,c);}
function E(){}
_=E.prototype=new cn();_.tI=0;function jb(b,a){return !(!(b&&qb[b][a]));}
function kb(a){return String.fromCharCode(a);}
function lb(b,a){if(b!=null)jb(b.tI,a)||pb();return b;}
function mb(b,a){return b!=null&&jb(b.tI,a);}
function nb(a){return a&65535;}
function pb(){throw new qm();}
function ob(a){if(a!==null){throw new qm();}return a;}
function rb(b,d){_=d.prototype;if(b&& !(b.tI>=_.tI)){var c=b.toString;for(var a in _){b[a]=_[a];}b.toString=c;}return b;}
var qb;function vb(){vb=ts;vc=gq(new eq());{pc=new Dd();he(pc);}}
function wb(b,a){vb();me(pc,b,a);}
function xb(a,b){vb();return be(pc,a,b);}
function yb(){vb();return oe(pc,'div');}
function zb(){vb();return pe(pc,'password');}
function Ab(a){vb();return ce(pc,a);}
function Bb(){vb();return pe(pc,'text');}
function Cb(){vb();return oe(pc,'label');}
function Db(){vb();return oe(pc,'span');}
function Eb(){vb();return oe(pc,'tbody');}
function Fb(){vb();return oe(pc,'td');}
function ac(){vb();return oe(pc,'tr');}
function bc(){vb();return oe(pc,'table');}
function ec(b,a,d){vb();var c;c=o;{dc(b,a,d);}}
function dc(b,a,c){vb();var d;if(a===uc){if(jc(b)==8192){uc=null;}}d=cc;cc=b;try{c.F(b);}finally{cc=d;}}
function fc(b,a){vb();qe(pc,b,a);}
function gc(a){vb();return re(pc,a);}
function hc(a){vb();return de(pc,a);}
function ic(a){vb();return ee(pc,a);}
function jc(a){vb();return se(pc,a);}
function kc(a){vb();fe(pc,a);}
function mc(a,b){vb();return ue(pc,a,b);}
function lc(a,b){vb();return te(pc,a,b);}
function nc(a){vb();return ve(pc,a);}
function oc(a){vb();return ge(pc,a);}
function qc(b,a){vb();return ie(pc,b,a);}
function rc(a){vb();var b,c;c=true;if(vc.b>0){b=ob(kq(vc,vc.b-1));if(!(c=null.nb())){fc(a,true);kc(a);}}return c;}
function sc(a){vb();if(uc!==null&&xb(a,uc)){uc=null;}je(pc,a);}
function tc(b,a){vb();we(pc,b,a);}
function wc(a){vb();uc=a;ke(pc,a);}
function yc(a,b,c){vb();ye(pc,a,b,c);}
function xc(a,b,c){vb();xe(pc,a,b,c);}
function zc(a,b){vb();ze(pc,a,b);}
function Ac(a,b){vb();Ae(pc,a,b);}
function Bc(a,b){vb();Be(pc,a,b);}
function Cc(b,a,c){vb();Ce(pc,b,a,c);}
function Dc(a,b){vb();le(pc,a,b);}
var cc=null,pc=null,uc=null,vc;function ad(a){if(mb(a,4)){return xb(this,lb(a,4));}return x(rb(this,Ec),a);}
function bd(){return y(rb(this,Ec));}
function Ec(){}
_=Ec.prototype=new v();_.eQ=ad;_.hC=bd;_.tI=8;function fd(a){return x(rb(this,cd),a);}
function gd(){return y(rb(this,cd));}
function cd(){}
_=cd.prototype=new v();_.eQ=fd;_.hC=gd;_.tI=9;function md(){md=ts;od=gq(new eq());{nd();}}
function nd(){md();sd(new id());}
var od;function kd(){while((md(),od).b>0){ob(kq((md(),od),0)).nb();}}
function ld(){return null;}
function id(){}
_=id.prototype=new cn();_.gb=kd;_.hb=ld;_.tI=10;function rd(){rd=ts;td=gq(new eq());Bd=gq(new eq());{xd();}}
function sd(a){rd();hq(td,a);}
function ud(){rd();var a,b;for(a=so(td);lo(a);){b=lb(mo(a),5);b.gb();}}
function vd(){rd();var a,b,c,d;d=null;for(a=so(td);lo(a);){b=lb(mo(a),5);c=b.hb();{d=c;}}return d;}
function wd(){rd();var a,b;for(a=so(Bd);lo(a);){b=ob(mo(a));null.nb();}}
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
function Be(c,a,b){while(a.firstChild){a.removeChild(a.firstChild);}if(b!=null){a.appendChild($doc.createTextNode(b));}}
function Ce(c,b,a,d){b.style[a]=d;}
function Cd(){}
_=Cd.prototype=new cn();_.tI=0;function be(c,a,b){return a==b;}
function ce(c,b){var a=$doc.createElement('INPUT');a.type='radio';a.name=b;return a;}
function de(b,a){return a.target||null;}
function ee(b,a){return a.relatedTarget||null;}
function fe(b,a){a.preventDefault();}
function ge(c,a){var b=a.parentNode;if(b==null){return null;}if(b.nodeType!=1)b=null;return b||null;}
function he(d){$wnd.__dispatchCapturedMouseEvent=function(b){if($wnd.__dispatchCapturedEvent(b)){var a=$wnd.__captureElem;if(a&&a.__listener){ec(b,a,a.__listener);b.stopPropagation();}}};$wnd.__dispatchCapturedEvent=function(a){if(!rc(a)){a.stopPropagation();a.preventDefault();return false;}return true;};$wnd.addEventListener('click',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('dblclick',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousedown',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mouseup',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousemove',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('mousewheel',$wnd.__dispatchCapturedMouseEvent,true);$wnd.addEventListener('keydown',$wnd.__dispatchCapturedEvent,true);$wnd.addEventListener('keyup',$wnd.__dispatchCapturedEvent,true);$wnd.addEventListener('keypress',$wnd.__dispatchCapturedEvent,true);$wnd.__dispatchEvent=function(b){var c,a=this;while(a&& !(c=a.__listener))a=a.parentNode;if(a&&a.nodeType!=1)a=null;if(c)ec(b,a,c);};$wnd.__captureElem=null;}
function ie(c,b,a){while(a){if(b==a){return true;}a=a.parentNode;if(a&&a.nodeType!=1){a=null;}}return false;}
function je(b,a){if(a==$wnd.__captureElem)$wnd.__captureElem=null;}
function ke(b,a){$wnd.__captureElem=a;}
function le(c,b,a){b.__eventBits=a;b.onclick=a&1?$wnd.__dispatchEvent:null;b.ondblclick=a&2?$wnd.__dispatchEvent:null;b.onmousedown=a&4?$wnd.__dispatchEvent:null;b.onmouseup=a&8?$wnd.__dispatchEvent:null;b.onmouseover=a&16?$wnd.__dispatchEvent:null;b.onmouseout=a&32?$wnd.__dispatchEvent:null;b.onmousemove=a&64?$wnd.__dispatchEvent:null;b.onkeydown=a&128?$wnd.__dispatchEvent:null;b.onkeypress=a&256?$wnd.__dispatchEvent:null;b.onkeyup=a&512?$wnd.__dispatchEvent:null;b.onchange=a&1024?$wnd.__dispatchEvent:null;b.onfocus=a&2048?$wnd.__dispatchEvent:null;b.onblur=a&4096?$wnd.__dispatchEvent:null;b.onlosecapture=a&8192?$wnd.__dispatchEvent:null;b.onscroll=a&16384?$wnd.__dispatchEvent:null;b.onload=a&32768?$wnd.__dispatchEvent:null;b.onerror=a&65536?$wnd.__dispatchEvent:null;b.onmousewheel=a&131072?$wnd.__dispatchEvent:null;}
function Fd(){}
_=Fd.prototype=new Cd();_.tI=0;function Dd(){}
_=Dd.prototype=new Fd();_.tI=0;function ek(b,a){fk(b,hk(b)+kb(45)+a);}
function fk(b,a){rk(b.m,a,true);}
function hk(a){return pk(a.m);}
function ik(b,a){jk(b,hk(b)+kb(45)+a);}
function jk(b,a){rk(b.m,a,false);}
function kk(d,b,a){var c=b.parentNode;if(!c){return;}c.insertBefore(a,b);c.removeChild(b);}
function lk(b,a){if(b.m!==null){kk(b,b.m,a);}b.m=a;}
function mk(b,a){qk(b.m,a);}
function nk(b,a){Dc(b.m,a|nc(b.m));}
function ok(a){return mc(a,'className');}
function pk(a){var b,c;b=ok(a);c=nn(b,32);if(c>=0){return sn(b,0,c);}return b;}
function qk(a,b){yc(a,'className',b);}
function rk(c,j,a){var b,d,e,f,g,h,i;if(c===null){throw hn(new gn(),'Null widget handle. If you are creating a composite, ensure that initWidget() has been called.');}j=tn(j);if(qn(j)==0){throw ym(new xm(),'Style names cannot be empty');}i=ok(c);e=on(i,j);while(e!=(-1)){if(e==0||ln(i,e-1)==32){f=e+qn(j);g=qn(i);if(f==g||f<g&&ln(i,f)==32){break;}}e=pn(i,j,e+1);}if(a){if(e==(-1)){if(qn(i)>0){i+=' ';}yc(c,'className',i+j);}}else{if(e!=(-1)){b=tn(sn(i,0,e));d=tn(rn(i,e+qn(j)));if(qn(b)==0){h=d;}else if(qn(d)==0){h=b;}else{h=b+' '+d;}yc(c,'className',h);}}}
function dk(){}
_=dk.prototype=new cn();_.tI=0;_.m=null;function ll(a){if(a.k){throw Bm(new Am(),"Should only call onAttach when the widget is detached from the browser's document");}a.k=true;zc(a.m,a);a.s();a.eb();}
function ml(a){if(!a.k){throw Bm(new Am(),"Should only call onDetach when the widget is attached to the browser's document");}try{a.fb();}finally{a.t();zc(a.m,null);a.k=false;}}
function nl(a){if(a.l!==null){a.l.jb(a);}else if(a.l!==null){throw Bm(new Am(),"This widget's parent does not implement HasWidgets");}}
function ol(b,a){if(b.k){zc(b.m,null);}lk(b,a);if(b.k){zc(a,b);}}
function pl(c,b){var a;a=c.l;if(b===null){if(a!==null&&a.k){c.db();}c.l=null;}else{if(a!==null){throw Bm(new Am(),'Cannot set a new parent without first clearing the old parent');}c.l=b;if(b.k){c.E();}}}
function ql(){}
function rl(){}
function sl(){ll(this);}
function tl(a){}
function ul(){ml(this);}
function vl(){}
function wl(){}
function xl(a){ol(this,a);}
function zk(){}
_=zk.prototype=new dk();_.s=ql;_.t=rl;_.E=sl;_.F=tl;_.db=ul;_.eb=vl;_.fb=wl;_.kb=xl;_.tI=11;_.k=false;_.l=null;function ti(b,a){pl(a,b);}
function vi(b,a){pl(a,null);}
function wi(){var a,b;for(b=this.B();Ek(b);){a=Fk(b);a.E();}}
function xi(){var a,b;for(b=this.B();Ek(b);){a=Fk(b);a.db();}}
function yi(){}
function zi(){}
function si(){}
_=si.prototype=new zk();_.s=wi;_.t=xi;_.eb=yi;_.fb=zi;_.tI=12;function xf(a){a.e=cl(new Ak(),a);}
function yf(a){xf(a);return a;}
function zf(c,a,b){nl(a);dl(c.e,a);wb(b,a.m);ti(c,a);}
function Bf(b,c){var a;if(c.l!==b){return false;}vi(b,c);a=c.m;tc(oc(a),a);jl(b.e,c);return true;}
function Cf(){return hl(this.e);}
function Df(a){return Bf(this,a);}
function wf(){}
_=wf.prototype=new si();_.B=Cf;_.jb=Df;_.tI=13;function Ee(a){yf(a);a.kb(yb());Cc(a.m,'position','relative');Cc(a.m,'overflow','hidden');return a;}
function Fe(a,b){zf(a,b,a.m);}
function bf(a){Cc(a,'left','');Cc(a,'top','');Cc(a,'position','');}
function cf(b){var a;a=Bf(this,b);if(a){bf(b.m);}return a;}
function De(){}
_=De.prototype=new wf();_.jb=cf;_.tI=14;function rh(){rh=ts;jm(),lm;}
function qh(b,a){jm(),lm;th(b,a);return b;}
function sh(b,a){switch(jc(a)){case 1:break;case 4096:case 2048:break;case 128:case 512:case 256:break;}}
function th(b,a){ol(b,a);nk(b,7041);}
function uh(){return !lc(this.m,'disabled');}
function vh(a){sh(this,a);}
function wh(a){th(this,a);}
function ph(){}
_=ph.prototype=new zk();_.A=uh;_.F=vh;_.kb=wh;_.tI=15;function ff(){ff=ts;jm(),lm;}
function ef(b,a){jm(),lm;qh(b,a);return b;}
function df(){}
_=df.prototype=new ph();_.tI=16;function hf(a){yf(a);a.d=bc();a.c=Eb();wb(a.d,a.c);a.kb(a.d);return a;}
function kf(c,b,a){yc(b,'align',a.a);}
function lf(c,b,a){Cc(b,'verticalAlign',a.a);}
function gf(){}
_=gf.prototype=new wf();_.tI=17;_.c=null;_.d=null;function of(){of=ts;jm(),lm;}
function nf(b,a){var c;jm(),lm;ef(b,Db());b.a=a;b.b=Cb();Dc(b.a,nc(b.m));Dc(b.m,0);wb(b.m,b.a);wb(b.m,b.b);c='check'+ ++vf;yc(b.a,'id',c);yc(b.b,'htmlFor',c);return b;}
function pf(b){var a;a=b.k?'checked':'defaultChecked';return lc(b.a,a);}
function qf(b,a){xc(b.a,'checked',a);xc(b.a,'defaultChecked',a);}
function rf(b,a){Bc(b.b,a);}
function sf(){return !lc(this.a,'disabled');}
function tf(){zc(this.a,this);}
function uf(){zc(this.a,null);qf(this,pf(this));}
function mf(){}
_=mf.prototype=new df();_.A=sf;_.eb=tf;_.fb=uf;_.tI=18;_.a=null;_.b=null;var vf=0;function mg(){mg=ts;jm(),lm;}
function jg(a,b){jm(),lm;ig(a);gg(a.h,b);return a;}
function kg(b,c,a){jm(),lm;jg(b,c);gg(rg(b),a);return b;}
function ig(a){jm(),lm;ef(a,Fl((nh(),oh)));nk(a,6269);ch(a,ng(a,null,'up',0));mk(a,'gwt-CustomButton');return a;}
function lg(a){if(a.f||a.g){sc(a.m);a.f=false;a.g=false;a.ab();}}
function ng(d,a,c,b){return ag(new Ff(),a,d,c,b);}
function og(a){if(a.a===null){Ag(a,a.h);}}
function pg(a){og(a);return a.a;}
function qg(a){if(a.d===null){Bg(a,ng(a,rg(a),'down-disabled',5));}return a.d;}
function rg(a){if(a.c===null){Cg(a,ng(a,a.h,'down',1));}return a.c;}
function sg(a){if(a.e===null){Dg(a,ng(a,rg(a),'down-hovering',3));}return a.e;}
function tg(b,a){switch(a){case 1:return rg(b);case 0:return b.h;case 3:return sg(b);case 2:return vg(b);case 4:return ug(b);case 5:return qg(b);default:throw Bm(new Am(),a+' is not a known face id.');}}
function ug(a){if(a.i===null){bh(a,ng(a,a.h,'up-disabled',4));}return a.i;}
function vg(a){if(a.j===null){dh(a,ng(a,a.h,'up-hovering',2));}return a.j;}
function wg(a){return (1&pg(a).a)>0;}
function xg(a){return (2&pg(a).a)>0;}
function Ag(b,a){if(b.a!==a){if(b.a!==null){ik(b,b.a.b);}b.a=a;yg(b,fg(a));ek(b,b.a.b);}}
function zg(c,a){var b;b=tg(c,a);Ag(c,b);}
function yg(b,a){if(b.b!==a){if(b.b!==null){tc(b.m,b.b);}b.b=a;wb(b.m,b.b);}}
function Eg(b,a){if(a!=wg(b)){eh(b);}}
function Bg(b,a){b.d=a;}
function Cg(b,a){b.c=a;}
function Dg(b,a){b.e=a;}
function Fg(b,a){if(a){gm((nh(),oh),b.m);}else{dm((nh(),oh),b.m);}}
function ah(b,a){if(a!=xg(b)){fh(b);}}
function bh(a,b){a.i=b;}
function ch(a,b){a.h=b;}
function dh(a,b){a.j=b;}
function eh(b){var a;a=pg(b).a^1;zg(b,a);}
function fh(b){var a;a=pg(b).a^2;a&=(-5);zg(b,a);}
function gh(){og(this);ll(this);}
function hh(a){var b,c;if(this.A()==false){return;}c=jc(a);switch(c){case 4:Fg(this,true);this.bb();wc(this.m);this.f=true;kc(a);break;case 8:if(this.f){this.f=false;sc(this.m);if(xg(this)){this.cb();}}break;case 64:if(this.f){kc(a);}break;case 32:if(qc(this.m,hc(a))&& !qc(this.m,ic(a))){if(this.f){this.ab();}ah(this,false);}break;case 16:if(qc(this.m,hc(a))){ah(this,true);if(this.f){this.bb();}}break;case 1:return;case 4096:if(this.g){this.g=false;this.ab();}break;case 8192:if(this.f){this.f=false;this.ab();}break;}sh(this,a);b=nb(gc(a));switch(c){case 128:if(b==32){this.g=true;this.bb();}break;case 512:if(this.g&&b==32){this.g=false;this.cb();}break;case 256:if(b==10||b==13){this.bb();this.cb();}break;}}
function kh(){}
function ih(){}
function jh(){}
function lh(){ml(this);lg(this);}
function Ef(){}
_=Ef.prototype=new df();_.E=gh;_.F=hh;_.cb=kh;_.ab=ih;_.bb=jh;_.db=lh;_.tI=19;_.a=null;_.b=null;_.c=null;_.d=null;_.e=null;_.f=false;_.g=false;_.h=null;_.i=null;_.j=null;function dg(c,a,b){c.e=b;c.c=a;return c;}
function fg(a){if(a.d===null){if(a.c===null){a.d=yb();return a.d;}else{return fg(a.c);}}else{return a.d;}}
function gg(b,a){b.d=yb();rk(b.d,'html-face',true);Bc(b.d,a);hg(b);}
function hg(a){if(a.e.a!==null&&fg(a.e.a)===fg(a)){yg(a.e,a.d);}}
function cg(){}
_=cg.prototype=new cn();_.tI=0;_.c=null;_.d=null;function ag(c,a,b,e,d){c.b=e;c.a=d;dg(c,a,b);return c;}
function Ff(){}
_=Ff.prototype=new cg();_.tI=0;function nh(){nh=ts;oh=(jm(),km);}
var oh;function pi(a){a.kb(yb());nk(a,131197);mk(a,'gwt-Label');return a;}
function ri(a){switch(jc(a)){case 1:break;case 4:case 8:case 64:case 16:case 32:break;case 131072:break;}}
function oi(){}
_=oi.prototype=new zk();_.F=ri;_.tI=20;function yh(a){pi(a);a.kb(yb());nk(a,125);mk(a,'gwt-HTML');return a;}
function zh(b,a){yh(b);Bh(b,a);return b;}
function Bh(b,a){Ac(b.m,a);}
function xh(){}
_=xh.prototype=new oi();_.tI=21;function ci(){ci=ts;ai(new Fh(),'center');di=ai(new Fh(),'left');ai(new Fh(),'right');}
var di;function ai(b,a){b.a=a;return b;}
function Fh(){}
_=Fh.prototype=new cn();_.tI=0;_.a=null;function ji(){ji=ts;hi(new gi(),'bottom');hi(new gi(),'middle');ki=hi(new gi(),'top');}
var ki;function hi(a,b){a.a=b;return a;}
function gi(){}
_=gi.prototype=new cn();_.tI=0;_.a=null;function Fj(){Fj=ts;jm(),lm;}
function Ej(b,a){jm(),lm;qh(b,a);nk(b,1024);return b;}
function ak(a){var b;sh(this,a);b=jc(a);}
function Dj(){}
_=Dj.prototype=new ph();_.F=ak;_.tI=22;function Ci(){Ci=ts;jm(),lm;}
function Bi(a){jm(),lm;Ej(a,zb());mk(a,'gwt-PasswordTextBox');return a;}
function Ai(){}
_=Ai.prototype=new Dj();_.tI=23;function aj(){aj=ts;jm(),lm;}
function Ei(a){{mk(a,'gwt-PushButton');}}
function Fi(b,c,a){jm(),lm;kg(b,c,a);Ei(b);return b;}
function dj(){Eg(this,false);}
function bj(){Eg(this,false);}
function cj(){Eg(this,true);}
function Di(){}
_=Di.prototype=new Ef();_.cb=dj;_.ab=bj;_.bb=cj;_.tI=24;function hj(){hj=ts;jm(),lm;}
function fj(b,a){jm(),lm;nf(b,Ab(a));mk(b,'gwt-RadioButton');return b;}
function gj(c,b,a){jm(),lm;fj(c,b);rf(c,a);return c;}
function ej(){}
_=ej.prototype=new mf();_.tI=25;function oj(){oj=ts;tj=ur(new Bq());}
function nj(b,a){oj();Ee(b);if(a===null){a=pj();}b.kb(a);b.E();return b;}
function qj(){oj();return rj(null);}
function rj(c){oj();var a,b;b=lb(Ar(tj,c),6);if(b!==null){return b;}a=null;if(tj.c==0){sj();}Br(tj,c,b=nj(new ij(),a));return b;}
function pj(){oj();return $doc.body;}
function sj(){oj();sd(new jj());}
function ij(){}
_=ij.prototype=new De();_.tI=26;var tj;function lj(){var a,b;for(b=lp(zp((oj(),tj)));sp(b);){a=lb(tp(b),6);if(a.k){a.db();}}}
function mj(){return null;}
function jj(){}
_=jj.prototype=new cn();_.gb=lj;_.hb=mj;_.tI=27;function ck(){ck=ts;jm(),lm;}
function bk(a){jm(),lm;Ej(a,Bb());mk(a,'gwt-TextBox');return a;}
function Cj(){}
_=Cj.prototype=new Dj();_.tI=28;function tk(a){a.a=(ci(),di);a.b=(ji(),ki);}
function uk(a){hf(a);tk(a);yc(a.d,'cellSpacing','0');yc(a.d,'cellPadding','0');return a;}
function vk(b,d){var a,c;c=ac();a=xk(b);wb(c,a);wb(b.c,c);zf(b,d,a);}
function xk(b){var a;a=Fb();kf(b,a,b.a);lf(b,a,b.b);return a;}
function yk(c){var a,b;b=oc(c.m);a=Bf(this,c);if(a){tc(this.c,oc(b));}return a;}
function sk(){}
_=sk.prototype=new gf();_.jb=yk;_.tI=29;function cl(b,a){b.a=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[4],null);return b;}
function dl(a,b){gl(a,b,a.b);}
function fl(b,c){var a;for(a=0;a<b.b;++a){if(b.a[a]===c){return a;}}return (-1);}
function gl(d,e,a){var b,c;if(a<0||a>d.b){throw new Dm();}if(d.b==d.a.a){c=fb('[Lcom.google.gwt.user.client.ui.Widget;',[0],[8],[d.a.a*2],null);for(b=0;b<d.a.a;++b){gb(c,b,d.a[b]);}d.a=c;}++d.b;for(b=d.b-1;b>a;--b){gb(d.a,b,d.a[b-1]);}gb(d.a,a,e);}
function hl(a){return Ck(new Bk(),a);}
function il(c,b){var a;if(b<0||b>=c.b){throw new Dm();}--c.b;for(a=b;a<c.b;++a){gb(c.a,a,c.a[a+1]);}gb(c.a,c.b,null);}
function jl(b,c){var a;a=fl(b,c);if(a==(-1)){throw new ps();}il(b,a);}
function Ak(){}
_=Ak.prototype=new cn();_.tI=0;_.a=null;_.b=0;function Ck(b,a){b.b=a;return b;}
function Ek(a){return a.a<a.b.b-1;}
function Fk(a){if(a.a>=a.b.b){throw new ps();}return a.b.a[++a.a];}
function al(){return Ek(this);}
function bl(){return Fk(this);}
function Bk(){}
_=Bk.prototype=new cn();_.z=al;_.D=bl;_.tI=0;_.a=(-1);function jm(){jm=ts;km=cm(new bm());lm=km!==null?im(new yl()):km;}
function im(a){jm();return a;}
function yl(){}
_=yl.prototype=new cn();_.tI=0;var km,lm;function Cl(){Cl=ts;jm();}
function Al(a){a.a=Dl(a);a.b=El(a);a.c=fm(a);}
function Bl(a){Cl();im(a);Al(a);return a;}
function Dl(b){return function(a){if(this.parentNode.onblur){this.parentNode.onblur(a);}};}
function El(b){return function(a){if(this.parentNode.onfocus){this.parentNode.onfocus(a);}};}
function Fl(c){var a=$doc.createElement('div');var b=c.r();b.addEventListener('blur',c.a,false);b.addEventListener('focus',c.b,false);a.addEventListener('mousedown',c.c,false);a.appendChild(b);return a;}
function am(){var a=$doc.createElement('input');a.type='text';a.style.width=a.style.height=0;a.style.zIndex= -1;a.style.position='absolute';return a;}
function zl(){}
_=zl.prototype=new yl();_.r=am;_.tI=0;function em(){em=ts;Cl();}
function cm(a){em();Bl(a);return a;}
function dm(b,a){$wnd.setTimeout(function(){a.firstChild.blur();},0);}
function fm(b){return function(){var a=this.firstChild;$wnd.setTimeout(function(){a.focus();},0);};}
function gm(b,a){$wnd.setTimeout(function(){a.firstChild.focus();},0);}
function hm(){var a=$doc.createElement('input');a.type='text';a.style.opacity=0;a.style.zIndex= -1;a.style.height='1px';a.style.width='1px';a.style.overflow='hidden';a.style.position='absolute';return a;}
function bm(){}
_=bm.prototype=new zl();_.r=hm;_.tI=0;function Cn(b,a){a;return b;}
function Bn(){}
_=Bn.prototype=new cn();_.tI=3;function vm(b,a){Cn(b,a);return b;}
function um(){}
_=um.prototype=new Bn();_.tI=4;function hn(b,a){vm(b,a);return b;}
function gn(){}
_=gn.prototype=new um();_.tI=5;function nm(){}
_=nm.prototype=new gn();_.tI=30;function qm(){}
_=qm.prototype=new gn();_.tI=31;function ym(b,a){hn(b,a);return b;}
function xm(){}
_=xm.prototype=new gn();_.tI=32;function Bm(b,a){hn(b,a);return b;}
function Am(){}
_=Am.prototype=new gn();_.tI=33;function Em(b,a){hn(b,a);return b;}
function Dm(){}
_=Dm.prototype=new gn();_.tI=34;function an(){}
_=an.prototype=new gn();_.tI=35;function ln(b,a){return b.charCodeAt(a);}
function nn(b,a){return b.indexOf(String.fromCharCode(a));}
function on(b,a){return b.indexOf(a);}
function pn(c,b,a){return c.indexOf(b,a);}
function qn(a){return a.length;}
function rn(b,a){return b.substr(a,b.length-a);}
function sn(c,a,b){return c.substr(a,b-a);}
function tn(c){var a=c.replace(/^(\s*)/,'');var b=a.replace(/\s*$/,'');return b;}
function un(a,b){return String(a)==b;}
function vn(a){if(!mb(a,1))return false;return un(this,a);}
function xn(){var a=wn;if(!a){a=wn={};}var e=':'+this;var b=a[e];if(b==null){b=0;var f=this.length;var d=f<64?1:f/32|0;for(var c=0;c<f;c+=d){b<<=1;b+=this.charCodeAt(c);}b|=0;a[e]=b;}return b;}
_=String.prototype;_.eQ=vn;_.hC=xn;_.tI=2;var wn=null;function An(a){return s(a);}
function Fn(b,a){hn(b,a);return b;}
function En(){}
_=En.prototype=new gn();_.tI=36;function co(d,a,b){var c;while(a.z()){c=a.D();if(b===null?c===null:b.eQ(c)){return a;}}return null;}
function fo(a){throw Fn(new En(),'add');}
function go(b){var a;a=co(this,this.B(),b);return a!==null;}
function bo(){}
_=bo.prototype=new cn();_.o=fo;_.q=go;_.tI=0;function ro(b,a){throw Em(new Dm(),'Index: '+a+', Size: '+b.b);}
function so(a){return jo(new io(),a);}
function to(b,a){throw Fn(new En(),'add');}
function uo(a){this.n(this.lb(),a);return true;}
function vo(e){var a,b,c,d,f;if(e===this){return true;}if(!mb(e,11)){return false;}f=lb(e,11);if(this.lb()!=f.lb()){return false;}c=so(this);d=f.B();while(lo(c)){a=mo(c);b=mo(d);if(!(a===null?b===null:a.eQ(b))){return false;}}return true;}
function wo(){var a,b,c,d;c=1;a=31;b=so(this);while(lo(b)){d=mo(b);c=31*c+(d===null?0:d.hC());}return c;}
function xo(){return so(this);}
function yo(a){throw Fn(new En(),'remove');}
function ho(){}
_=ho.prototype=new bo();_.n=to;_.o=uo;_.eQ=vo;_.hC=wo;_.B=xo;_.ib=yo;_.tI=37;function jo(b,a){b.c=a;return b;}
function lo(a){return a.a<a.c.lb();}
function mo(a){if(!lo(a)){throw new ps();}return a.c.x(a.b=a.a++);}
function no(a){if(a.b<0){throw new Am();}a.c.ib(a.b);a.a=a.b;a.b=(-1);}
function oo(){return lo(this);}
function po(){return mo(this);}
function io(){}
_=io.prototype=new cn();_.z=oo;_.D=po;_.tI=0;_.a=0;_.b=(-1);function xp(f,d,e){var a,b,c;for(b=pr(f.u());ir(b);){a=jr(b);c=a.v();if(d===null?c===null:d.eQ(c)){if(e){kr(b);}return a;}}return null;}
function yp(b){var a;a=b.u();return Bo(new Ao(),b,a);}
function zp(b){var a;a=zr(b);return jp(new ip(),b,a);}
function Ap(a){return xp(this,a,false)!==null;}
function Bp(d){var a,b,c,e,f,g,h;if(d===this){return true;}if(!mb(d,12)){return false;}f=lb(d,12);c=yp(this);e=f.C();if(!bq(c,e)){return false;}for(a=Do(c);ep(a);){b=fp(a);h=this.y(b);g=f.y(b);if(h===null?g!==null:!h.eQ(g)){return false;}}return true;}
function Cp(b){var a;a=xp(this,b,false);return a===null?null:a.w();}
function Dp(){var a,b,c;b=0;for(c=pr(this.u());ir(c);){a=jr(c);b+=a.hC();}return b;}
function Ep(){return yp(this);}
function zo(){}
_=zo.prototype=new cn();_.p=Ap;_.eQ=Bp;_.y=Cp;_.hC=Dp;_.C=Ep;_.tI=38;function bq(e,b){var a,c,d;if(b===e){return true;}if(!mb(b,13)){return false;}c=lb(b,13);if(c.lb()!=e.lb()){return false;}for(a=c.B();a.z();){d=a.D();if(!e.q(d)){return false;}}return true;}
function cq(a){return bq(this,a);}
function dq(){var a,b,c;a=0;for(b=this.B();b.z();){c=b.D();if(c!==null){a+=c.hC();}}return a;}
function Fp(){}
_=Fp.prototype=new bo();_.eQ=cq;_.hC=dq;_.tI=39;function Bo(b,a,c){b.a=a;b.b=c;return b;}
function Do(b){var a;a=pr(b.b);return cp(new bp(),b,a);}
function Eo(a){return this.a.p(a);}
function Fo(){return Do(this);}
function ap(){return this.b.a.c;}
function Ao(){}
_=Ao.prototype=new Fp();_.q=Eo;_.B=Fo;_.lb=ap;_.tI=40;function cp(b,a,c){b.a=c;return b;}
function ep(a){return a.a.z();}
function fp(b){var a;a=b.a.D();return a.v();}
function gp(){return ep(this);}
function hp(){return fp(this);}
function bp(){}
_=bp.prototype=new cn();_.z=gp;_.D=hp;_.tI=0;function jp(b,a,c){b.a=a;b.b=c;return b;}
function lp(b){var a;a=pr(b.b);return qp(new pp(),b,a);}
function mp(a){return yr(this.a,a);}
function np(){return lp(this);}
function op(){return this.b.a.c;}
function ip(){}
_=ip.prototype=new bo();_.q=mp;_.B=np;_.lb=op;_.tI=0;function qp(b,a,c){b.a=c;return b;}
function sp(a){return a.a.z();}
function tp(a){var b;b=a.a.D().w();return b;}
function up(){return sp(this);}
function vp(){return tp(this);}
function pp(){}
_=pp.prototype=new cn();_.z=up;_.D=vp;_.tI=0;function fq(a){{iq(a);}}
function gq(a){fq(a);return a;}
function hq(b,a){xq(b.a,b.b++,a);return true;}
function iq(a){a.a=z();a.b=0;}
function kq(b,a){if(a<0||a>=b.b){ro(b,a);}return tq(b.a,a);}
function lq(b,a){return mq(b,a,0);}
function mq(c,b,a){if(a<0){ro(c,a);}for(;a<c.b;++a){if(sq(b,tq(c.a,a))){return a;}}return (-1);}
function nq(c,a){var b;b=kq(c,a);vq(c.a,a,1);--c.b;return b;}
function pq(a,b){if(a<0||a>this.b){ro(this,a);}oq(this.a,a,b);++this.b;}
function qq(a){return hq(this,a);}
function oq(a,b,c){a.splice(b,0,c);}
function rq(a){return lq(this,a)!=(-1);}
function sq(a,b){return a===b||a!==null&&a.eQ(b);}
function uq(a){return kq(this,a);}
function tq(a,b){return a[b];}
function wq(a){return nq(this,a);}
function vq(a,c,b){a.splice(c,b);}
function xq(a,b,c){a[b]=c;}
function yq(){return this.b;}
function eq(){}
_=eq.prototype=new ho();_.n=pq;_.o=qq;_.q=rq;_.x=uq;_.ib=wq;_.lb=yq;_.tI=41;_.a=null;_.b=0;function wr(){wr=ts;Dr=ds();}
function tr(a){{vr(a);}}
function ur(a){wr();tr(a);return a;}
function vr(a){a.a=z();a.d=A();a.b=rb(Dr,v);a.c=0;}
function xr(b,a){if(mb(a,1)){return hs(b.d,lb(a,1))!==Dr;}else if(a===null){return b.b!==Dr;}else{return gs(b.a,a,a.hC())!==Dr;}}
function yr(a,b){if(a.b!==Dr&&fs(a.b,b)){return true;}else if(cs(a.d,b)){return true;}else if(as(a.a,b)){return true;}return false;}
function zr(a){return nr(new er(),a);}
function Ar(c,a){var b;if(mb(a,1)){b=hs(c.d,lb(a,1));}else if(a===null){b=c.b;}else{b=gs(c.a,a,a.hC());}return b===Dr?null:b;}
function Br(c,a,d){var b;{b=c.b;c.b=d;}if(b===Dr){++c.c;return null;}else{return b;}}
function Cr(c,a){var b;if(mb(a,1)){b=ks(c.d,lb(a,1));}else if(a===null){b=c.b;c.b=rb(Dr,v);}else{b=js(c.a,a,a.hC());}if(b===Dr){return null;}else{--c.c;return b;}}
function Er(e,c){wr();for(var d in e){if(d==parseInt(d)){var a=e[d];for(var f=0,b=a.length;f<b;++f){c.o(a[f]);}}}}
function Fr(d,a){wr();for(var c in d){if(c.charCodeAt(0)==58){var e=d[c];var b=Fq(c.substring(1),e);a.o(b);}}}
function as(f,h){wr();for(var e in f){if(e==parseInt(e)){var a=f[e];for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.w();if(fs(h,d)){return true;}}}}return false;}
function bs(a){return xr(this,a);}
function cs(c,d){wr();for(var b in c){if(b.charCodeAt(0)==58){var a=c[b];if(fs(d,a)){return true;}}}return false;}
function ds(){wr();}
function es(){return zr(this);}
function fs(a,b){wr();if(a===b){return true;}else if(a===null){return false;}else{return a.eQ(b);}}
function is(a){return Ar(this,a);}
function gs(f,h,e){wr();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.v();if(fs(h,d)){return c.w();}}}}
function hs(b,a){wr();return b[':'+a];}
function js(f,h,e){wr();var a=f[e];if(a){for(var g=0,b=a.length;g<b;++g){var c=a[g];var d=c.v();if(fs(h,d)){if(a.length==1){delete f[e];}else{a.splice(g,1);}return c.w();}}}}
function ks(c,a){wr();a=':'+a;var b=c[a];delete c[a];return b;}
function Bq(){}
_=Bq.prototype=new zo();_.p=bs;_.u=es;_.y=is;_.tI=42;_.a=null;_.b=null;_.c=0;_.d=null;var Dr;function Dq(b,a,c){b.a=a;b.b=c;return b;}
function Fq(a,b){return Dq(new Cq(),a,b);}
function ar(b){var a;if(mb(b,14)){a=lb(b,14);if(fs(this.a,a.v())&&fs(this.b,a.w())){return true;}}return false;}
function br(){return this.a;}
function cr(){return this.b;}
function dr(){var a,b;a=0;b=0;if(this.a!==null){a=this.a.hC();}if(this.b!==null){b=this.b.hC();}return a^b;}
function Cq(){}
_=Cq.prototype=new cn();_.eQ=ar;_.v=br;_.w=cr;_.hC=dr;_.tI=43;_.a=null;_.b=null;function nr(b,a){b.a=a;return b;}
function pr(a){return gr(new fr(),a.a);}
function qr(c){var a,b,d;if(mb(c,14)){a=lb(c,14);b=a.v();if(xr(this.a,b)){d=Ar(this.a,b);return fs(a.w(),d);}}return false;}
function rr(){return pr(this);}
function sr(){return this.a.c;}
function er(){}
_=er.prototype=new Fp();_.q=qr;_.B=rr;_.lb=sr;_.tI=44;function gr(c,b){var a;c.c=b;a=gq(new eq());if(c.c.b!==(wr(),Dr)){hq(a,Dq(new Cq(),null,c.c.b));}Fr(c.c.d,a);Er(c.c.a,a);c.a=so(a);return c;}
function ir(a){return lo(a.a);}
function jr(a){return a.b=lb(mo(a.a),14);}
function kr(a){if(a.b===null){throw Bm(new Am(),'Must call next() before remove().');}else{no(a.a);Cr(a.c,a.b.v());a.b=null;}}
function lr(){return ir(this);}
function mr(){return jr(this);}
function fr(){}
_=fr.prototype=new cn();_.z=lr;_.D=mr;_.tI=0;_.a=null;_.b=null;function ps(){}
_=ps.prototype=new gn();_.tI=45;function vs(a){a.e=bk(new Cj());a.c=Bi(new Ai());a.b=gj(new ej(),'interfaceRadioGroup','MOD Curator');a.d=gj(new ej(),'interfaceRadioGroup','Reference Genome');a.a=Fi(new Di(),'Login','Login');}
function ws(a){vs(a);return a;}
function ys(a){var b;b=uk(new sk());vk(b,zh(new xh(),'<h2>Login<\/h2>'));vk(b,a.e);vk(b,a.c);vk(b,a.d);vk(b,a.b);vk(b,a.a);return b;}
function us(){}
_=us.prototype=new cn();_.tI=0;function Bs(b){var a;a=ws(new us());Fe(qj(),ys(a));}
function zs(){}
_=zs.prototype=new cn();_.tI=0;function mm(){Bs(new zs());}
function gwtOnLoad(b,d,c){$moduleName=d;$moduleBase=c;if(b)try{mm();}catch(a){b(d);}else{mm();}}
var qb=[{},{},{1:1},{3:1},{3:1},{3:1},{3:1},{2:1},{2:1,4:1},{2:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{6:1,7:1,8:1,9:1,10:1},{5:1},{7:1,8:1,9:1,10:1},{7:1,8:1,9:1,10:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{3:1},{11:1},{12:1},{13:1},{13:1},{11:1},{12:1},{14:1},{13:1},{3:1}];if (org_bbop_RefGenome) {  var __gwt_initHandlers = org_bbop_RefGenome.__gwt_initHandlers;  org_bbop_RefGenome.onScriptLoad(gwtOnLoad);}})();