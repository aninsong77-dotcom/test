window.__zabu = function(){
  const T=THREE; const g=new T.Group(); scene.add(g);
  // 기존 방석 숨기기 (자리 판정은 그대로)
  domeTap.forEach(m=>{ if(m.userData.tap && m.userData.tap.kind==='seat'){ m.visible=false; } });
  const w=0.74, r=0.12, sh=new T.Shape(); sh.moveTo(-w/2+r,-w/2); sh.lineTo(w/2-r,-w/2); sh.quadraticCurveTo(w/2,-w/2,w/2,-w/2+r); sh.lineTo(w/2,w/2-r); sh.quadraticCurveTo(w/2,w/2,w/2-r,w/2); sh.lineTo(-w/2+r,w/2); sh.quadraticCurveTo(-w/2,w/2,-w/2,w/2-r); sh.lineTo(-w/2,-w/2+r); sh.quadraticCurveTo(-w/2,-w/2,-w/2+r,-w/2);
  const geo=new T.ExtrudeGeometry(sh,{depth:0.03,bevelEnabled:true,bevelThickness:0.065,bevelSize:0.07,bevelSegments:8,curveSegments:12}); geo.rotateX(-Math.PI/2); geo.translate(0,0.065,0);
  const pa=geo.attributes.position; for(let i=0;i<pa.count;i++){ const x=pa.getX(i), z=pa.getZ(i), y=pa.getY(i); if(y>0.08){ const d=Math.max(Math.abs(x),Math.abs(z))/0.42; pa.setY(i,y+0.035*Math.max(0,1-d*d)); } } geo.computeVertexNormals();
  // 바닥은 은은한 초록 단색
  const grassMat=new T.MeshStandardMaterial({color:'#a9cf94',roughness:0.95});
  dome.children.forEach(o=>{ const gp=o.geometry&&o.geometry.parameters; if(o.isMesh&&o.geometry.type==='CylinderGeometry'&&gp.radiusTop>2.8) o.material=grassMat; });
  const PASTEL=['#f6c6c0','#c8e6d4','#d6cdf0','#fbe3b6','#c6dff3','#f3d1e3'];
  SEATS.forEach((st,i)=>{ const zg=new T.Group(); const col = PASTEL[i%PASTEL.length];
    zg.add(new T.Mesh(geo,new T.MeshStandardMaterial({color:col,roughness:0.95})));
    const tuft=new T.Mesh(new T.SphereGeometry(0.03,10,8),new T.MeshStandardMaterial({color:new T.Color(col).offsetHSL(0,0,-0.18),roughness:0.9})); tuft.scale.y=0.5; tuft.position.y=0.17; zg.add(tuft);
    zg.position.set(st.x, HOUSE_BASE+0.17, st.z); zg.rotation.y=st.heading; g.add(zg); });
};
