shinyUI(
  fluidPage(
  HTML('<HTML><HEAD><TITLE>REV- 2: Calculation of the RT50 for Vaccines</TITLE>
    <META http-equiv=Content-Type content="text/html; charset=iso-8859-1">
    <META content="Pouillot R&#233;gis" name=Author>
    <META content="MSHTML 5.50.4134.600" name=GENERATOR>
    <META content="REV - 2 : Calculation of the RT50 for Vaccines" 
  name=KeyWords></HEAD>
    <BODY>&nbsp;<FONT size=+3><FONT 
  color=#008080><p></p>REV-2</FONT> <FONT color=#000080>: Calculation of the RT 50 for 
    vaccines
  <HR>
    </FONT></FONT> REV - 2 evaluates and compares RT 50 values for <I>Brucella</I> vaccines using the Probit model. 
  <P>For more details : see "<a href="http://www.oie.int/doc/ged/d572.pdf">Statistical procedures for calculating the residual virulence (Recovery Time 50 -RT50-) of <i>Brucella abortus</i> S19 and <i>Brucella melitensis</i> Rev 1 vaccines in mice: theoretical basis and practical applications.</a>"
  from R. Pouillot*, M.J. Grill&#243;**, J.L. Alabart**, B. Garin-Bastuji*** and J.M. Blasco**, <i>Revue Scientifique et technique de l&#39;OIE</i>, 2003, 22 (3), 1051-1063.<p></p>
    <small>* Agence Fran&#231;aise de S&#233;curit&#233; Sanitaire des Aliments, Unit&#233; d&#39;Appui Epid&#233;miologique &#224; l&#39;Analyse de Risque, BP19, F-94701 Maison-Alfort Cedex (France).<br>
    ** Unidad de Sanidad Animal, Servicio Investigaci&#243;n Agroalimentaria, Diputaci&#243;n General de Arag&#243;n. Apartado 727. 50080. Zaragoza (Spain).<br>
    *** Agence Fran&#231;aise de S&#233;curit&#233; Sanitaire des Aliments, Unit&#233; des Zoonoses Bact&#233;riennes, BP19, F-94701 Maison-Alfort Cedex (France).
    </small> 
    
    <p></p><P>Just follow the line ... 
  <P><FONT color=#000080 size=+1>1. Complete the table:</FONT></B> 
    <UL>
    <LI>NAME : Name of the vaccine for identification purpose 
  <LI>TIME : Point-time of slaughter (by default : 3, 6, 9, 12 weeks) 
  <LI>TREATED : Number of treated mice at each point time of slaughter (by 
                                                                        default : 8 animals. Change this number if some animals died during the 
                                                                        experiment) 
  <LI>CURED : Number of cleared mice (at each point-time of slaughter, no 
                                      correction needed if CURED=0 or CURED=TREATED) </LI></UL>
    <P></FONT>
    <CENTER>
    <TABLE cellSpacing=4 cellPadding=4 border=1>
    <TBODY>
    <TR>
    <TD colSpan=3><FONT color=#008080 size=+1>Reference</FONT></TD>
    <TD><I>&nbsp;&nbsp;&nbsp;&nbsp;vs&nbsp;&nbsp;&nbsp;&nbsp;</I></TD>
    <TD colSpan=3><FONT color=#008080 size=+1>Tested</FONT></TD></TR>
    <TR>
    <TD><FONT color=#008080 size=+1>Name</FONT></TD>
    <TD colSpan=2>'),

textInput("nom1","",value="REV 1"), 
HTML('</TD>
    <TD></TD>
    <TD><FONT color=#008080 size=+1>Name</FONT></TD>
    <TD colSpan=2>'),
textInput("nom2","",value="Tested"),
HTML('</TD></TR>
    <TR>
    <TD><FONT color=#000080>&nbsp;TIME&nbsp;</FONT></TD>
    <TD><FONT color=#000080>&nbsp;TREATED&nbsp;</FONT></TD>
    <TD><FONT color=#000080>&nbsp;CURED&nbsp;</FONT></TD>
    <TD></TD>
    <TD><FONT color=#000080>&nbsp;TIME&nbsp;</FONT></TD>
    <TD><FONT color=#000080>&nbsp;TREATED&nbsp;</FONT></TD>
    <TD><FONT color=#000080>&nbsp;CURED&nbsp;</FONT></TD></TR>
    <TR>
    <TD>
    <CENTER>3</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n1","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n2","",value=0),
HTML('</CENTER></TD>
    <TD></TD>
    <TD>
    <CENTER>3</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n3","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n4","",value=0),
HTML('</CENTER></TD></TR>
    <TR>
    <TD>
    <CENTER>6</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n5","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n6","",value=0),
HTML('</CENTER></TD>
    <TD></TD>
    <TD>
    <CENTER>6</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n7","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n8","",value=0),
HTML('</CENTER></TD></TR>
    <TR>
    <TD>
    <CENTER>9</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n9","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n10","",value=0),
HTML('</CENTER></TD>
    <TD></TD>
    <TD>
    <CENTER>9</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n11","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n12","",value=0),
HTML('</CENTER></TD></TR>
    <TR>
    <TD>
    <CENTER>12</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n13","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n14","",value=0),
HTML('</CENTER></TD>
    <TD></TD>
    <TD>
    <CENTER>12</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n15","",value=8),
HTML('</CENTER></TD>
    <TD>
    <CENTER>'),
numericInput("n16","",value=0),
HTML('</CENTER></TR></TBODY></TABLE>
    <P></CENTER>'),
HTML('<FONT color=#000080 size=+1>2. Check this box if you want 
                                    detailled statistics:</FONT>'),
checkboxInput("ouinon",label="",value=TRUE),
HTML('
    <FONT color=#000080 size=+1>3. 
    Submit your data: 
    <P>
    </FONT>'),
actionButton(inputId = "soume", label="Click here"),
hr(),
uiOutput("Results"),
hr(),
plotOutput("Graph1"),
plotOutput("Graph2"),
plotOutput("Graph3"),
HTML('
    <HR>
    <ADDRESS>Last update: November 2014. Please send all comments to <A 
  href="mailto:bruno.garin-bastuji@anses.fr">Bruno Garin-Bastuji</A>
    or <A 
  href="mailto:rpouillot@yahoo.fr">R&#233;gis Pouillot</A></ADDRESS>
    <HR>'),
hr()

)#EndFluidPage
)
  