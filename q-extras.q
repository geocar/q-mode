
plot:{x:{$[50>count x;x;x floor count[x]*til[50]%50]}each x;
  R:reverse 6 10*system"c";n:3|10&first[R]div 6;b:max each x;c:{reverse til[count x]%count x}each x;
  p:raze(-1_'.h.hta[`polyline;]each([]fill:count[x]#enlist"none";stroke:"#",'string count[x]#`7fb148`58595b`6e75b5`f28030`22b6f0;points:{" "sv","sv'x}each string(c*first R),''(x%b)*last R)),\:"/>";
  t:.h.htac[`svg;`version`xmlns`viewBox!("1.2";"http://www.w3.org/2000/svg";" "sv string 0 0,R)]p;
  atob:{c:(neg count x)mod 3;(neg[c]_.Q.b6[raze -4#',[0 0 0 0;]each 64 vs'256 sv'"i"$0N 3#x,c#0]),c#"="}; //.Q.atob
  // q-mode supports a limited form of iterm-style embedded images https://iterm2.com/documentation-images.html
  -1"\033]1337;File=name=",atob[string[.z.z],".svg"],";size=",string[count t],";width=100%;inline=1:",atob[t],"\007\n";
  };

