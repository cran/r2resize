document.querySelectorAll(".no-js").forEach(t=>{t.classList.remove("no-js")});const splitContainersc87n767m08o=document.querySelectorAll(".r2resize-c87n767m08o-splitcard2-split-container");function touchOrMousePosition(t){return/^touch/.test(t.type)?t.touches[0].clientX:t.clientX}splitContainersc87n767m08o.forEach(t=>{var e=0,n=t.getBoundingClientRect().left,i=t.clientWidth,o=!1;let s=t.querySelector(".r2resize-c87n767m08o-splitcard2-split-content-right"),c=document.createElement("div");function l(s){n=t.getBoundingClientRect().left,i=t.clientWidth,u(e=(e=touchOrMousePosition(s)-n)<0?0:e>i?i:e),d(e),c.classList.add("active"),o=!0}function r(t){o&&(t.preventDefault(),u(e=(e=touchOrMousePosition(t)-n)<0?0:e>i?i:e),d(e))}function a(t){c.classList.remove("active"),o=!1}function u(t){let e=`polygon(${t}px 0%, 100% 0%, 100% 100%, ${t}px 100%)`;s.style.clipPath=e,s.style.webkitClipPath=e}function d(t){c.style.marginLeft=t+"px"}c.className="r2resize-c87n767m08o-splitcard2-separator",c.style.marginLeft="slidepos1232",t.appendChild(c),t.addEventListener("touchstart",l,!1),t.addEventListener("mousedown",l,!1),t.addEventListener("touchend",a,!1),t.addEventListener("mouseup",a,!1),t.addEventListener("touchmove",r,!1),t.addEventListener("mousemove",r,!1)});
