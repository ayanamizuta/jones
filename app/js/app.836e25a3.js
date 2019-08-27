(function(t){function e(e){for(var r,s,o=e[0],c=e[1],u=e[2],f=0,_=[];f<o.length;f++)s=o[f],a[s]&&_.push(a[s][0]),a[s]=0;for(r in c)Object.prototype.hasOwnProperty.call(c,r)&&(t[r]=c[r]);h&&h(e);while(_.length)_.shift()();return i.push.apply(i,u||[]),n()}function n(){for(var t,e=0;e<i.length;e++){for(var n=i[e],r=!0,o=1;o<n.length;o++){var c=n[o];0!==a[c]&&(r=!1)}r&&(i.splice(e--,1),t=s(s.s=n[0]))}return t}var r={},a={app:0},i=[];function s(e){if(r[e])return r[e].exports;var n=r[e]={i:e,l:!1,exports:{}};return t[e].call(n.exports,n,n.exports,s),n.l=!0,n.exports}s.m=t,s.c=r,s.d=function(t,e,n){s.o(t,e)||Object.defineProperty(t,e,{enumerable:!0,get:n})},s.r=function(t){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},s.t=function(t,e){if(1&e&&(t=s(t)),8&e)return t;if(4&e&&"object"===typeof t&&t&&t.__esModule)return t;var n=Object.create(null);if(s.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:t}),2&e&&"string"!=typeof t)for(var r in t)s.d(n,r,function(e){return t[e]}.bind(null,r));return n},s.n=function(t){var e=t&&t.__esModule?function(){return t["default"]}:function(){return t};return s.d(e,"a",e),e},s.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},s.p="/";var o=window["webpackJsonp"]=window["webpackJsonp"]||[],c=o.push.bind(o);o.push=e,o=o.slice();for(var u=0;u<o.length;u++)e(o[u]);var h=c;i.push([0,"chunk-vendors"]),n()})({0:function(t,e,n){t.exports=n("56d7")},"034f":function(t,e,n){"use strict";var r=n("64a9"),a=n.n(r);a.a},2337:function(t,e,n){"use strict";var r=n("5be3"),a=n.n(r);a.a},"56d7":function(t,e,n){"use strict";n.r(e);n("cadf"),n("551c"),n("f751"),n("097d");var r=n("2b0e"),a=function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("div",{attrs:{id:"app"}},[n("h1",[t._v("Kauffman Bracket")]),n("DrawManager",{ref:"draw_manager"}),t.configured&&null==t.kauffman_bracket?n("button",{on:{click:function(e){return t.submit()}}},[t._v("submit")]):t._e(),t.kauffman_bracket?n("h2",{attrs:{id:"kauffman_bracket"}},[t._v("The Kauffman bracket of this link is \\("+t._s(t.kauffman_bracket)+"\\)")]):t._e()],1)},i=[],s=n("67b0"),o=function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("div",{on:{mouseup:function(e){return t.suspend()},mousedown:function(e){return t.resume()},click:function(e){return t.intersection_parity_change()}}},[n("Canvas",{ref:"canvas"}),"Done"==t.state?n("h3",[t._v("You've made a link diagram! congrats!")]):t._e(),"Done"==t.state?n("button",{on:{click:function(e){return t.configure_link()}}},[t._v("configure a link!")]):t._e(),"Configured"==t.state?n("h3",[t._v("You can submit this link and get the Kauffman bracket!")]):t._e()],1)},c=[],u=n("75fc"),h=(n("55dd"),n("ac6a"),n("c5f6"),function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("canvas",{staticClass:"canvas",attrs:{width:"600",height:"400"},on:{mousedown:function(e){return t.draw_activate()},mousemove:function(e){return t.get_current_position(e)},mouseup:function(e){return t.draw_deactivate()}}})}),f=[],_={data:function(){return{last_draw_x:null,last_draw_y:null,current_x:null,current_y:null,draw_enable:!0}},methods:{draw_activate:function(){document.addEventListener("mousemove",this.draw)},draw_deactivate:function(){document.removeEventListener("mousemove",this.draw)},get_current_position:function(t){var e=t.offsetX,n=t.offsetY;this.current_x=e,this.current_y=n},draw:function(){this.draw_enable&&(null==this.last_draw_x||null==this.last_draw_y?(this.last_draw_x=this.current_x,this.last_draw_y=this.current_y):(this.draw_core(this.last_draw_x,this.last_draw_y,this.current_x,this.current_y),this.last_draw_x=this.current_x,this.last_draw_y=this.current_y))},draw_core:function(t,e,n,r){this.ctx.beginPath(),this.ctx.moveTo(t,e),this.ctx.lineTo(n,r),this.ctx.stroke()}},mounted:function(){this.ctx=this.$el.getContext("2d")}},l=_,d=(n("2337"),n("2877")),p=Object(d["a"])(l,h,f,!1,null,"4f28fa70",null),m=p.exports,v={name:"draw_manager",props:{minimum_radius_of_loop:{type:Number,default:30},minimum_distance_for_resume:{type:Number,default:30},erase_radius:{type:Number,default:15}},data:function(){return{state:"Ready",state_sub:"DrawUnTerminable",initial_place:null,intersection_pairs:[],trajectory:[]}},components:{Canvas:m},methods:{intersection_parity_change:function(){if("Configured"==this.state){var t=this.$refs.canvas;this.intersection_pairs.forEach(function(e,n){var r=this.intersection_position(e[0],e[1]);if(this.distance(r,[t.current_x,t.current_y])<this.erase_radius){var a=[e[1],e[0]];this.intersection_pairs[n]=a}},this),this.rearrange_projection_diagram()}},dump_link_json:function(){var t=[];this.intersection_pairs.forEach(function(e){Array.prototype.push.apply(t,e)}),t.sort(function(t,e){return t-e});var e=t.length,n=[];function r(t){return 180*Math.atan2(t[1],t[0])/Math.PI}return this.intersection_pairs.forEach(function(a,i){var s=t.indexOf(a[0]),o=t.indexOf(a[1]),c=[this.trajectory[a[1]+1][0]-this.trajectory[a[1]][0],this.trajectory[a[1]+1][1]-this.trajectory[a[1]][1]],u=[this.trajectory[a[0]][0]-this.trajectory[a[1]][0],this.trajectory[a[0]][1]-this.trajectory[a[1]][1]],h=-r(c)+r(u),f={intersection_id:i,front_edge_ids:[o,(o+1)%e],back_edge_ids:[s,(s+1)%e]};if(h<0&&h>-180||h<360&&h>180){var _=f["back_edge_ids"][0];f["back_edge_ids"][0]=f["back_edge_ids"][1],f["back_edge_ids"][1]=_}n.push(f)},this),JSON.stringify(n)},distance:function(t,e){return Math.sqrt((t[0]-e[0])*(t[0]-e[0])+(t[1]-e[1])*(t[1]-e[1]))},resume:function(){var t=this.$refs.canvas;"Suspend"==this.state&&this.distance([t.current_x,t.current_y],[t.last_draw_x,t.last_draw_y])<this.minimum_distance_for_resume&&(this.$refs.canvas.draw_enable=!0,this.state="Draw")},suspend:function(){"Done"!=this.state&&"Configured"!=this.state&&(this.state="Suspend",this.$refs.canvas.draw_enable=!1)},is_intersect:function(t,e){var n=this.trajectory[t],r=this.trajectory[t+1],a=this.trajectory[e],i=this.trajectory[e+1],s=function(t){return t[1]-n[1]-(t[0]-n[0])*(r[1]-n[1])/(r[0]-n[0])};return s(a)*s(i)<0},intersection_position:function(t,e){var n=this.trajectory[t],r=this.trajectory[t+1],a=this.trajectory[e],i=this.trajectory[e+1],s=(n[1]-a[1]-n[0]*(r[1]-n[1])/(r[0]-n[0])+a[0]*(i[1]-a[1])/(i[0]-a[0]))/((i[1]-a[1])/(i[0]-a[0])-(r[1]-n[1])/(r[0]-n[0]));return[s,n[1]+s*(r[1]-n[1])/(r[0]-n[0])-n[0]*(r[1]-n[1])/(r[0]-n[0])]},rearrange_projection_diagram:function(){this.intersection_pairs.forEach(function(t){var e=this.intersection_position(t[0],t[1]);this.$refs.canvas.ctx.clearRect(e[0]-this.erase_radius/2,e[1]-this.erase_radius/2,this.erase_radius,this.erase_radius),Object(u["a"])(Array(5).keys()).forEach(function(e){var n=this.trajectory[Math.min(t[1]+e,this.trajectory.length-2)],r=this.trajectory[Math.min(t[1]+e,this.trajectory.length-2)+1];this.$refs.canvas.ctx.beginPath(),this.$refs.canvas.ctx.moveTo(n[0],n[1]),this.$refs.canvas.ctx.lineTo(r[0],r[1]),this.$refs.canvas.ctx.stroke(),n=this.trajectory[Math.max(0,t[1]-e)],r=this.trajectory[Math.max(0,t[1]-e)+1],this.$refs.canvas.ctx.beginPath(),this.$refs.canvas.ctx.moveTo(n[0],n[1]),this.$refs.canvas.ctx.lineTo(r[0],r[1]),this.$refs.canvas.ctx.stroke()},this)},this),this.state="Configured"},configure_link:function(){for(var t=this.trajectory.length,e=0;e<t-1;e++)for(var n=e+2;n<t-1;n++)this.is_intersect(e,n)&&this.is_intersect(n,e)&&this.intersection_pairs.push([e,n]);this.rearrange_projection_diagram()}},mounted:function(){this.$watch("$refs.canvas.last_draw_x",function(){var t=this.$refs.canvas.last_draw_x,e=this.$refs.canvas.last_draw_y;this.trajectory.push([t,e]),"Ready"==this.state?(this.state="Draw",this.state_sub="DrawUnTerminable",this.initial_place=[t,e]):"Draw"==this.state&&this.distance(this.initial_place,[t,e])>this.minimum_radius_of_loop?this.state_sub="DrawTerminable":"DrawTerminable"==this.state_sub&&this.distance(this.initial_place,[t,e])<this.minimum_radius_of_loop&&(this.state="Done",this.$refs.canvas.draw_core(t,e,this.initial_place[0],this.initial_place[1]),this.$refs.canvas.draw_enable=!1)})}},b=v,y=Object(d["a"])(b,o,c,!1,null,null,null),g=y.exports,w=n("bc3a"),x=n.n(w);r["a"].use(s["a"]),r["a"].loadScript("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML").then(function(){MathJax.Hub.Config({messageStyle:"none"})});var j={name:"app",data:function(){return{configured:!1,kauffman_bracket:null,url:"http://localhost:8080/jones/post"}},methods:{submit:function(){var t={headers:{"Content-Type":"application/json"},withCredentials:!0};x.a.post(this.url,this.$refs.draw_manager.dump_link_json(),t).then(function(t){this.kauffman_bracket=t.data,this.$nextTick(function(){return MathJax.Hub.Queue(["Typeset",MathJax.Hub,"kauffman_bracket"])})}.bind(this)).catch(function(t){alert(t)})}},mounted:function(){this.$watch("$refs.draw_manager.state",function(){this.configured="Configured"==this.$refs.draw_manager.state})},components:{DrawManager:g}},k=j,$=(n("034f"),Object(d["a"])(k,a,i,!1,null,null,null)),M=$.exports;r["a"].config.productionTip=!1,new r["a"]({render:function(t){return t(M)}}).$mount("#app")},"5be3":function(t,e,n){},"64a9":function(t,e,n){}});
//# sourceMappingURL=app.836e25a3.js.map