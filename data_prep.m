(* ::Package:: *)

(* ::Section:: *)
(*Making the graph*)


(* ::Chapter:: *)
(*Collectors, No Munqati, No Mursal, No Mubham*)


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
(*AbsoluteTiming[sr=SQLSelect[conn, "isnads_sanadrowah"]]*)


(* ::Input:: *)
(*dat=sr[[;;,1;;5]]*)


(* ::Input:: *)
(*dat=Select[dat,#[[5]]!=30001&];*)
(*dat//Length*)


(* ::Subsection:: *)
(*Create Edges with collectors and with the Prophet*)


(* ::Input:: *)
(*eswithcollectors=Flatten[*)
(*Table[*)
(*If[dat[[j-1,1]]==dat[[j,1]],*)
(*DirectedEdge[dat[[j-1,5]],dat[[j,5]]],DirectedEdge[99999,dat[[j,5]]] (* add the prophet *)*)
(*]*)
(*,{j,2,Length[dat]-1}]*)
(*];*)


(* ::Input:: *)
(*Length[eswithcollectors]*)


(* ::Input:: *)
(*AbsoluteTiming[eswithcollectors=Select[eswithcollectors,(#[[1]]!=30000)&&(#[[1]]!=30002)&&(#[[2]]!=30000)&&(#[[2]]!=30002)&];]*)


(* ::Input:: *)
(*AbsoluteTiming[eswithcollectors=Select[eswithcollectors,(#[[1]]!=1131)&&(#[[2]]!=1131)&];]*)


(* ::Input:: *)
(*eswithcollectors[[1]]*)


(* ::Input:: *)
(*Length[eswithcollectors]*)


(* ::Item:: *)
(*the graph is now made*)


(* ::Input:: *)
(*eswithcollectors=Flatten[eswithcollectors]*)


(* ::Input:: *)
(*AbsoluteTiming[gwcno3ksno1131=Graph[eswithcollectors];]*)


(* ::Input:: *)
(*sgwcno3ksn1131=SimpleGraph[gwcno3ksno1131];*)


(* ::Input:: *)
(*VertexCount[gwcno3ksno1131]*)


(* ::Input:: *)
(*EdgeCount[gwcno3ksno1131]*)


(* ::Subsection:: *)
(*Deleting edges with dd discrepancy of -30 or less*)


(* ::Input:: *)
(*highDeathEdges=SQLExecute[conn,"Select * from hadith.edges_death_discrepency where*)
(*(death_discrepency<-30 and death_discrepency>-99999)"]*)


(* ::Input:: *)
(*Length[highDeathEdges]*)


(* ::Input:: *)
(*highDeathEdges=Table[{DirectedEdge[highDeathEdges[[i,2]],highDeathEdges[[i,3]]]},{i,1,Length[highDeathEdges],1}]*)


(* ::Input:: *)
(*Length[highDeathEdges]*)


(* ::Input:: *)
(*highDeathEdges=DeleteDuplicates[highDeathEdges];*)


(* ::Input:: *)
(*Length[highDeathEdges]*)


(* ::Input:: *)
(*eswithcollectors=DeleteDuplicates[eswithcollectors]*)


(* ::Input:: *)
(*AbsoluteTiming[updatedGwcno3ksno1131=Graph[eswithcollectors];]*)


(* ::Input:: *)
(*updatedSgwcno3ksn1131=SimpleGraph[updatedGwcno3ksno1131];*)


(* ::Input:: *)
(*VertexCount[updatedSgwcno3ksn1131]*)


(* ::Input:: *)
(*EdgeCount[updatedSgwcno3ksn1131]*)


(* ::Input:: *)
(*highDeathEdges[[1]]*)


(* ::Input:: *)
(*Position[eswithcollectors,highDeathEdges[[1]]//.{x_}:>x]*)


(* ::Input:: *)
(*eswithcollectors[[109]]*)


(* ::Input:: *)
(*Length[highDeathEdges]*)


(* ::Input:: *)
(*Length[eswithcollectors]*)


(* ::Input:: *)
(*For[i=1,i<=Length[highDeathEdges],i++,eswithcollectors=DeleteCases[eswithcollectors,highDeathEdges[[i]]//.{x_}:>x]]*)


(* ::Item:: *)
(*this is the new updated graph after the high death date edges are deleted*)


(* ::Input:: *)
(*Length[eswithcollectors]*)


(* ::Subsection:: *)
(*Deleting bidirectional edges*)


(* ::Input:: *)
(*AbsoluteTiming[biDirEdges=SQLSelect[conn, "bi_directional_edges"]];*)


(* ::Input:: *)
(*biDirEdges[[2]]*)


(* ::Input:: *)
(*biDirEdges=Flatten[Table[{DirectedEdge[biDirEdges[[i,1]],biDirEdges[[i,2]]]},{i,1,Length[biDirEdges],1}]];*)


(* ::Input:: *)
(*biDirEdges[[1]]*)


(* ::Input:: *)
(*Length[eswithcollectors]*)


(* ::Input:: *)
(*For[i=1,i<=Length[biDirEdges],i++,eswithcollectors=DeleteCases[eswithcollectors,biDirEdges[[i]]//.{x_}:>x]]*)


(* ::Item:: *)
(*this is the new updated graph after the bidirectional edges are deleted*)


(* ::Input:: *)
(*Length[eswithcollectors]*)


(* ::Section:: *)
(*Connected Components to Narrow Down Edges*)


(* ::Item:: *)
(*there are many connected components; however, we're only interested in the ones with more than one narrator - that's where the cycles happen*)


(* ::Item:: *)
(*the connected components with only one transmitter represents the transmitters that are not involved in cycles - we can delete those transmitters from the graph and all their edges*)


(* ::Input:: *)
(*connectedComponents=ConnectedComponents[eswithcollectors]*)


(* ::Input:: *)
(*connectedComponents2={}*)


(* ::Item:: *)
(*this takes all the connected components that are greater than length one and stores them in connectedComponents2*)


(* ::Input:: *)
(*Table[{If[Length[connectedComponents[[i]]]>1,connectedComponents2=Insert[connectedComponents2,connectedComponents[[i]],Length[connectedComponents2]+1],""]},{i,1,Length[connectedComponents],1}];*)


(* ::Input:: *)
(*Length[connectedComponents2]*)


(* ::Input:: *)
(*connectedComponents2[[2]]*)


(* ::Input:: *)
(*connectedComponents2[[1]]*)


(* ::Input:: *)
(*connectedComponents2=Flatten[connectedComponents2]*)


(* ::Input:: *)
(*denseComponent=connectedComponents2*)


(* ::Item:: *)
(*this is the graph before*)


(* ::Input:: *)
(*updatedCollecters=eswithcollectors*)


(* ::Item:: *)
(*these are the amount of narrators before*)


(* ::Input:: *)
(*narratorsBefore=DeleteDuplicates[Flatten[Join[eswithcollectors[[;;,1]],eswithcollectors[[;;,2]]]]]*)


(* ::Item:: *)
(*these are the amount of edges before*)


(* ::Input:: *)
(*modifiedEdges=Table[{eswithcollectors[[i,1]],eswithcollectors[[i,2]]},{i,1,Length[eswithcollectors],1}]*)


(* ::Item:: *)
(*I'm now getting rid of all the edges that are not involved in connected components*)


(* ::Input:: *)
(*Monitor[graphWithStrongEdges=Flatten[Table[{If[MemberQ[denseComponent,modifiedEdges[[i,1]]]&&MemberQ[denseComponent,modifiedEdges[[i,2]]],eswithcollectors[[i]],Unevaluated[Sequence[]]]},{i,1,Length[modifiedEdges],1}]],i]*)


(* ::Input:: *)
(*graphAfter=graphWithStrongEdges*)


(* ::Item:: *)
(*this is the new graph with the critical edges (the ones that are involved in cycles)*)


(* ::Input:: *)
(*graphAfter*)


(* ::Item:: *)
(*these are the narrators involved in cycles*)


(* ::Input:: *)
(*narratorsAfter=DeleteDuplicates[Flatten[Join[graphWithStrongEdges[[;;,1]],graphWithStrongEdges[[;;,2]]]]]*)


(* ::Section:: *)
(*Changing the format of the input to insert into Johnson's algorithm (in python)*)


(* ::Input:: *)
(*IncidenceList[graphAfter,4883];*)


(* ::Input:: *)
(*one={VertexList[graphAfter][[1]],IncidenceList[graphAfter,VertexList[graphAfter][[1]]]};*)


(* ::Input:: *)
(*modifyInput={};*)


(* ::Input:: *)
(*For[i=1,i<=10,i++,*)
(*one={VertexList[graphAfter][[i]],IncidenceList[graphAfter,VertexList[graphAfter][[i]]]};*)
(*one[[2]]=Select[one[[2]][[;;,2]],#!=one[[1]]&];*)
(*modifyInput=Insert[modifyInput,{one},Length[modifyInput]+1]];*)


(* ::Input:: *)
(*one={VertexList[graphAfter][[3]],IncidenceList[graphAfter,VertexList[graphAfter][[3]]]}*)


(* ::Input:: *)
(*one[[2]]=DeleteDuplicates[one[[2]][[;;,2]]]*)


(* ::Input:: *)
(*shortened=graphAfter[[;;1000]];*)


(* ::Input:: *)
(*modifyInput={}*)


(* ::Input:: *)
(*Monitor[For[i=1,i<=Length[VertexList[shortened]],i++,*)
(*one={VertexList[shortened][[i]],IncidenceList[shortened,VertexList[shortened][[i]]]};*)
(*one[[2]]=Select[one[[2]][[;;,2]],#!=one[[1]]&];*)
(*modifyInput=Append[modifyInput,{one}]],i]*)


(* ::Input:: *)
(*modifyInput[[2]]*)


(* ::Input:: *)
(*Length[modifyInput]*)


(* ::Input:: *)
(*modifyInput[[1,1,2]]*)


(* ::Input:: *)
(*Length[modifyInput[[1,1,2]]]*)


(* ::Input:: *)
(*s="{"*)


(* ::Input:: *)
(*s=StringJoin[s,ToString["25"]]*)


(* ::Input:: *)
(*input=""*)


(* ::Item:: *)
(*this will work,now lets try another simpler graph for testing*)


(* ::Input:: *)
(*input=StringJoin[input,ToString["{"]];*)


(* ::Input:: *)
(*For[i=1,i<=Length[modifyInput],i++,*)
(*input=StringJoin[input,ToString[modifyInput[[i,1,1]]]];*)
(*input=StringJoin[input,ToString[": ["]];*)
(*For[j=1,j<=Length[modifyInput[[i,1,2]]],j++,*)
(*input=StringJoin[input,ToString[modifyInput[[i,1,2,j]]]];*)
(*If[j<Length[modifyInput[[i,1,2]]],input=StringJoin[input,", "]];*)
(*];*)
(*input=StringJoin[input,ToString["], "]];]*)


(* ::Input:: *)
(*input=StringJoin[input,ToString["}"]];*)


(* ::Input:: *)
(*Length[modifyInput[[1,1,2]]]*)


(* ::Input:: *)
(*input;*)
