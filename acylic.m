(* ::Package:: *)

(* ::Section:: *)
(*Import the Reduced Graph*)


(* ::Input:: *)
(*Needs["DatabaseLink`"];*)


(* ::Input:: *)
(*JDBCDrivers["MySQL(Connector/J)"]*)


(* ::Input:: *)
(*JDBCDriver["Name"->"MySQL(Connector/J)","Driver"->"com.mysql.jdbc.Driver","Protocol"->"jdbc:mysql://","Version"->3.1`,"Description"->"MySQL using Connector/J - Version 5.1.38","Location"->"/Applications/Mathematica.app/Contents/SystemFiles/Links/DatabaseLink/DatabaseResources/mysql.m"]*)


(* ::Input:: *)
(*<<JLink`;*)
(*InstallJava[];*)
(*ReinstallJava[JVMArguments->"-Xmx4048m"]*)


(* ::Input:: *)
(*conn=OpenSQLConnection[JDBC["MySQL(Connector/J)","isnad-team-786.c5umbdflqwtu.us-west-1.rds.amazonaws.com/hadith"],"Username"->"danny","Password"->"danny"]*)


(* ::Input:: *)
(*graphAfter=Import["/home/mairaj/Documents/Cycles3/graphAfter.csv"]*)


(* ::Input:: *)
(*graphAfter=Flatten[ToExpression[graphAfter]]*)


(* ::Section:: *)
(*Creating Associations for Crit*)


(* ::Input:: *)
(*AbsoluteTiming[icddgd=SQLSelect[conn, "icddgd"]]*)


(* ::Input:: *)
(*gd=Table[{DirectedEdge[icddgd[[i,1]],icddgd[[i,2]]],icddgd[[i,5]]},{i,1,Length[icddgd],1}]*)


(* ::Input:: *)
(*ddichctcbc=ToExpression[Import["/home/mairaj/Documents/Cycles3/precycleedgeinfo.csv"]]*)


(* ::Input:: *)
(*hc=Flatten[Table[{ddichctcbc[[i,1]]->ddichctcbc[[i,4]]},{i,1,Length[ddichctcbc],1}]]*)


(* ::Input:: *)
(*hc=Association[hc]*)


(* ::Input:: *)
(*ic=Flatten[Table[{ddichctcbc[[i,1]]->ddichctcbc[[i,3]]},{i,1,Length[ddichctcbc],1}]]*)


(* ::Input:: *)
(*ic=Association[ic]*)


(* ::Input:: *)
(*dd=Sort[Flatten[Table[{ddichctcbc[[i,1]]->ddichctcbc[[i,2]]},{i,1,Length[ddichctcbc],1}]]]*)


(* ::Input:: *)
(*dd=SortBy[dd,Last]*)


(* ::Input:: *)
(*dd=Association[dd]*)


(* ::Input:: *)
(*gd=Flatten[Table[{gd[[i,1]]->gd[[i,2]]},{i,1,Length[gd],1}]]*)


(* ::Input:: *)
(*gd=SortBy[gd,Last]*)


(* ::Input:: *)
(*gd[[53277]]*)


(* ::Input:: *)
(*gd[[53278]]*)


(* ::Input:: *)
(*Table[{gd[[j,2]]=0},{j,1,53277,1}];*)


(* ::Input:: *)
(*gd=Association[gd]*)


(* ::Input:: *)
(*bC=Flatten[Table[{ddichctcbc[[i,1]]->ddichctcbc[[i,6]]},{i,1,Length[ddichctcbc],1}]]*)


(* ::Input:: *)
(*bC=Association[bC]*)


(* ::Input:: *)
(*bC[2\[DirectedEdge]415]*)


(* ::Input:: *)
(*tC=Flatten[Table[{ddichctcbc[[i,1]]->ddichctcbc[[i,5]]},{i,1,Length[ddichctcbc],1}]]*)


(* ::Input:: *)
(*tC=Association[tC]*)


(* ::Input:: *)
(*tC[2\[DirectedEdge]415]*)


(* ::Section:: *)
(*Algorithm*)


(* ::Input:: *)
(*(**this is the lsit that store the edges we delete and their info**)*)


(* ::Input:: *)
(*edgs={}*)


(* ::Input:: *)
(*(**this is our graph that has the edges we've added*)*)


(* ::Input:: *)
(*partGraph={}*)


(* ::Input:: *)
(*partGraph*)


(* ::Input:: *)
(*partGraph=Flatten[ToExpression[Import["/home/mairaj/Documents/Cycles3/29958/graphReduced8fromr23987.csv","CSV"]]]*)


(* ::Input:: *)
(*Length[partGraph]*)


(* ::Input:: *)
(*edgs=ToExpression[Import["/home/mairaj/Documents/Cycles3/22958/edgs8fromr22813.csv","CSV"]]*)


(* ::Input:: *)
(*edgs=DeleteDuplicates[edgs]*)


(* ::Input:: *)
(*partGraph=DeleteDuplicates[partGraph]*)


(* ::Input:: *)
(*notadded=Import["/home/mairaj/Documents/Cycles3/notadded.csv","CSV"]*)


(* ::Input:: *)
(*notadded=ToExpression[notadded]*)


(* ::Input:: *)
(*notadded=Flatten[notadded]*)


(* ::Input:: *)
(*graphaftertemp=graphAfter;*)
(*graphAfter=notadded*)


(* ::Input:: *)
(*GraphEdgeList[edgelist0_,highlightedge0_,edgeinfo0_]:=Module[{edgelist=edgelist0,highlightedge=highlightedge0,edgeinfo=edgeinfo0},*)
(*cyc=Association[Table[edgeinfo[[i,1]]-> edgeinfo[[i,2]],{i,1,Length[edgeinfo]}]];*)
(*vertexlabels=Table["v: "<>ToString[VertexList[edgelist][[i]]]<>"\rd. "<> ToString[ biosA[VertexList[edgelist][[i]]][[31]]],{i,1,VertexCount[edgelist]}];*)
(**)
(*edgelabels=Table["d: "<>ToString[dd[EdgeList[edgelist][[i]]]]<>", g: "<>ToString[gd[EdgeList[edgelist][[i]]]]<>"\rcyc: "<>  ToString[cyc[EdgeList[edgelist][[i]]]] <>  ", hc: " <> ToString[hc[EdgeList[edgelist][[i]]]]<>"\rbc: " <> ToString[bC[EdgeList[edgelist][[i]]]]<>", tc: " <> ToString[tC[EdgeList[edgelist][[i]]]],{i,1,EdgeCount[edgelist]}];*)
(**)
(*Graph[edgelist,ImageSize->Large,GraphLayout->"LayeredDigraphEmbedding",VertexLabels->Table[VertexList[edgelist][[i]]->vertexlabels[[i]],{i,1,VertexCount[edgelist]}],VertexSize->Tiny,GraphHighlight->{highlightedge}, EdgeLabels->Table[edgelist[[i]]->edgelabels[[i]],{i,1,EdgeCount[edgelist]}],EdgeLabelStyle->Directive[Red,Italic,8]]]*)


(* ::Input:: *)
(*r=23987*)


(* ::Input:: *)
(*Length[partGraph]*)


(* ::Input:: *)
(*Last[partGraph]*)


(* ::Input:: *)
(*ccofpartgraph=ConnectedComponents[partGraph]*)


(* ::Input:: *)
(*VertexCount[partGraph]*)


(* ::Input:: *)
(*Length[ccofpartgraph]*)


(* ::Input:: *)
(*FindCycle[partGraph]*)


(* ::Input:: *)
(*r*)


(* ::Input:: *)
(*badEdge*)


(* ::Input:: *)
(*Monitor[For[r=r,r<=Length[graphAfter],r++,*)
(*(**append edge to graph and check for cycles**)*)
(*partGraph=Append[partGraph,graphAfter[[r]]];*)
(**)
(*partGraph=Flatten[partGraph];*)
(**)
(*partGraphTemp=Graph[partGraph];*)
(*If[AcyclicGraphQ[partGraphTemp],Continue[]];*)
(**)
(*cycles=TimeConstrained[FindCycle[partGraph,Infinity,All],30];*)
(*Print[Length[cycles]];*)
(**)
(*If[ToString[cycles]=="$Aborted",{badEdge=graphAfter[[r]],partGraph=Delete[partGraph,Length[partGraph]],edgs=Append[edgs,badEdge],Print["aborted"],Print[badEdge],Export["/home/mairaj/Documents/Cycles3/22813/edgs8fromr22813.csv",edgs,"CSV"],*)
(*Export["/home/mairaj/Documents/Cycles3/22813/edgs8fromr22813.tsv",edgs,"TSV"],*)
(*Export["/home/mairaj/Documents/Cycles3/22813/graphReduced8fromr22813.csv",partGraph,"CSV"],*)
(*Export["/home/mairaj/Documents/Cycles3/22813/graphReduced8fromr22813.tsv",partGraph,"TSV"],*)
(*Export["/home/mairaj/Documents/Cycles3/22813/r8from22813.csv",r,"CSV"],*)
(*Export["/home/mairaj/Documents/Cycles3/22813/lengthofpartGraph8fromr22813.csv",Length[partGraph],"CSV"],*)
(*Export["/home/mairaj/Documents/Cycles3/22813/lengthofedgs8fromr22813.csv",Length[edgs],"CSV"],Continue[]}];*)
(**)
(*Print["After first if"];*)
(**)
(*(**if there's no cycles we continue**)*)
(*(*If[Length[cycles]\[Equal]0,Continue[]];*)*)
(*(**this finds the connected component**)*)
(*connectedComponents=ConnectedComponents[partGraph];*)
(*component={};*)
(*If[Length[cycles]>0,For[i=1,i<=Length[connectedComponents],i++,If[Length[connectedComponents[[i]]]>1,component=Append[component,connectedComponents[[i]]]]]];*)
(*edges=DeleteDuplicates[Sort[Flatten[cycles]]];*)
(*edgesAll=Sort[Flatten[cycles]];*)
(*(**this counts the number of cycles each edge is involved in**)*)
(*occurencesOfEdges={};*)
(*occurencesOfEdges=Table[{edges[[i]],0,0},{i,1,Length[edges],1}];*)
(*j=1;TimeConstrained[Monitor[For[i=1,i<=Length[edges],i++,*)
(*For[j,j<=Length[edgesAll]&&occurencesOfEdges[[i,1]]==edgesAll[[j]] ,j++,*)
(*occurencesOfEdges[[i,2]]++;]],j],100];*)
(*For[i=1,i<=Length[occurencesOfEdges],i++,occurencesOfEdges[[i,3]]=occurencesOfEdges[[i,2]]/Length[cycles]];*)
(*occurencesOfEdges=Sort[occurencesOfEdges];*)
(*(**this adds all the information about each edge, hc, dd, gd**)*)
(*edgeInfo=Table[{occurencesOfEdges[[i,1]],occurencesOfEdges[[i,2]],occurencesOfEdges[[i,3]],hc[occurencesOfEdges[[i,1]]],dd[occurencesOfEdges[[i,1]]],gd[occurencesOfEdges[[i,1]]],bC[occurencesOfEdges[[i,1]]],tC[occurencesOfEdges[[i,1]]]},{i,1,Length[occurencesOfEdges],1}];*)
(*edgeInfo=Table[{Flatten[edgeInfo[[i]]]},{i,1,Length[edgeInfo],1}];*)
(*edgeInfo=edgeInfo//.{x_List}:>x;*)
(*(**this sorts all the different categories**)*)
(*sortedByCycles=SortBy[edgeInfo,#[[3]]&];*)
(*sortedByHC=Reverse[SortBy[edgeInfo,#[[4]]&]];*)
(*sortedByDD=Reverse[SortBy[edgeInfo,#[[5]]&]];*)
(*sortedByG=Reverse[SortBy[edgeInfo,#[[6]]&]];*)
(*sortedByBC=Reverse[SortBy[edgeInfo,#[[7]]&]];*)
(*sortedByTC=Reverse[SortBy[edgeInfo,#[[8]]&]];*)
(*(**this ranks all the edges**)*)
(*rank=Table[{edges[[i]],0},{i,1,Length[edges],1}];cycleScore=0;cycleBias=1;hadithCountScore=0;hadithCountBias=1;deathDateScore=0;deathDateBias=.75;genScore=0;genBias=.50;bookCountScore=0;bookCountBias=1;*)
(*tarafCountScore=0;*)
(*tarafCountBias=1.25;*)
(*For[i=1,i<=Length[rank],i++,*)
(*For[j=1,j<=Length[rank] ,j++,*)
(*If[rank[[i,1]]==sortedByCycles[[j,1]],{For[z=j,z>1&&sortedByCycles[[z-1,3]]==sortedByCycles[[j,3]],z--],cycleScore=z;}];*)
(*If[rank[[i,1]]==sortedByHC[[j,1]],{For[z=j,z>1&&sortedByHC[[z-1,4]]==sortedByHC[[j,4]],z--],hadithCountScore=z;}];*)
(*If[rank[[i,1]]==sortedByDD[[j,1]],{For[z=j,z>1&&sortedByDD[[z-1,5]]==sortedByDD[[j,5]],z--],deathDateScore=z;}];*)
(*If[rank[[i,1]]==sortedByG[[j,1]],{For[z=j,z>1&&sortedByG[[z-1,6]]==sortedByG[[j,6]],z--],genScore=z;}];*)
(*If[rank[[i,1]]==sortedByBC[[j,1]],{For[z=j,z>1&&sortedByBC[[z-1,7]]==sortedByBC[[j,7]],z--],bookCountScore=z;}];*)
(*If[rank[[i,1]]==sortedByTC[[j,1]],{For[z=j,z>1&&sortedByTC[[z-1,8]]==sortedByTC[[j,8]],z--],tarafCountScore=z;}];];*)
(*rank[[i,2]]=(cycleScore*cycleBias)+(hadithCountScore*hadithCountBias)+(deathDateScore*deathDateBias)+(genScore*genBias)+(bookCountScore*bookCountBias)+(tarafCountScore*tarafCountBias);*)
(*];*)
(*(**normalizing the ranks**)*)
(*Table[{rank[[i,2]]=(rank[[i,2]]/((Length[rank]*cycleBias)+(Length[rank]*hadithCountBias)+(Length[rank]*deathDateBias)+(Length[rank]*genBias)+(Length[rank]*bookCountBias)+(Length[rank]*tarafCountBias)))},{i,1,Length[rank],1}];*)
(*rank=SortBy[rank,#[[2]]&];*)
(*(**this sorts the edgeInfo by the rank**)*)
(*edgeInfoRanked={};*)
(*For[i=1,i<=Length[rank],i++,edgeInfoRanked=Append[edgeInfoRanked,Flatten[{edgeInfo[[Position[edgeInfo,rank[[i,1]]][[1,1]]]],rank[[i,2]]}]]];*)
(*(**this gets the crit./crux edges**)*)
(*cruxEdges={};*)
(*For[i=1,i<=Length[edges],i++,*)
(*temp=FindCycle[Flatten[Append[edges[[;;i-1]],edges[[i+1;;Length[edges]]]]]];*)
(*If[Length[temp]==0,cruxEdges=Append[cruxEdges,edges[[i]]]]];*)
(*(**this displays the component in hiearchical representation**)*)
(*(**TimeConstrained[Print[GraphEdgeList[edges,edgeInfoRanked[[Length[edgeInfoRanked],1]],edgeInfoRanked]],10];**)*)
(*badEdge=0;*)
(*For[i=Length[edgeInfoRanked],badEdge==0&&i>0,i--,If[MemberQ[cruxEdges,edgeInfoRanked[[i,1]]],badEdge=edgeInfoRanked[[i,1]]]];*)
(*If[badEdge==0,badEdge=edgeInfoRanked[[Length[edgeInfoRanked],1]]];*)
(*(**Print[cruxEdges];*)
(*Print[edgeInfoRanked];**)*)
(*Print[badEdge];*)
(*Print[r];*)
(*edgs=Append[edgs,{badEdge,edges,component,edgeInfoRanked}];*)
(**)
(**)
(*(*If[Mod[r,1000]\[Equal]0||r\[Equal]Length[notadded]-1,SendMail["dhalawi@berkeley.edu",ToString["This is the value of r: "]<>ToString[r]<>ToString["\nThis is the length of the part graph: "]<>ToString[Length[partGraph]]<>ToString["\nThis is the number of edges removed so far: "]<>ToString[Length[edgs]]<>ToString["\nThis is the length of edges not added yet: "]<>ToString[Length[notadded]]]];*)*)
(**)
(**)
(**)
(*(**this removes the edge and adds it to the list of susp. edges**)*)
(*posOfBE=Position[partGraph,badEdge];*)
(*partGraph=Delete[partGraph,posOfBE[[1,1]]];*)
(**)
(*Export["/home/mairaj/Documents/Cycles3/22813/edgs8fromr22813.csv",edgs,"CSV"];*)
(*Export["/home/mairaj/Documents/Cycles3/22813/edgs8fromr22813.tsv",edgs,"TSV"];*)
(*Export["/home/mairaj/Documents/Cycles3/22813/graphReduced8fromr22813.csv",partGraph,"CSV"];*)
(*Export["/home/mairaj/Documents/Cycles3/22813/graphReduced8fromr22813.tsv",partGraph,"TSV"];*)
(*Export["/home/mairaj/Documents/Cycles3/22813/r8from22813.csv",r,"CSV"];*)
(*Export["/home/mairaj/Documents/Cycles3/22813/lengthofpartGraph8fromr22813.csv",Length[partGraph],"CSV"];*)
(*Export["/home/mairaj/Documents/Cycles3/22813/lengthofedgs8fromr22813.csv",Length[edgs],"CSV"];*)
(*(** if we want to manually pick the edges use the code below**)*)
(*(**(**this asks the user for the number of edges to delete**)*)
(*numOfEdgesToRemove=ToExpression[InputString["How many edges would you like to remove"]];*)
(*For[i=1,i\[LessEqual]numOfEdgesToRemove,i++,*)
(*(**this asks the user for the bad edge**)*)
(*badEdge=ToExpression[InputString[edgeInfoRanked]];*)
(*comments=InputString["additonal comments?"];*)
(*revR=Reverse[edgeInfoRanked];*)
(*badEdge=revR[[badEdge,1]];*)
(*edgs=Append[edgs,{badEdge,component,edgeInfoRanked,rank,edges,comments}];*)
(*(**this removes the edge and adds it to the list of susp. edges**)*)
(*posOfBE=Position[partGraph,badEdge];*)
(*partGraph=Delete[partGraph,posOfBE[[1,1]]];]**)*)
(*];,r]*)



