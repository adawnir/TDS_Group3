### TDS Project -- Make legend
## Programme created by Rin on 21 March 

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts"
setwd(project_path)


## Load labels
plot_annot=read_csv("../Dictionaries/plot_annot.csv")
plot_annot=plot_annot[-c(1,2,21:25),] # Remove age, sex and BMI

mycolours=c("grey50" ,"tomato","forestgreen","royalblue","gold")

## Make label
foo=function(x,n){
  x=paste0(n," - ",x)
  if (grepl("m\\^", x)){
    x=gsub("m\\^","'~m^", x)
    x=sub(")","~')", x)
  }
  if (grepl("\\[", x)){
    x=gsub("\\[","'[", x)
    x=sub("\\(ug","~'(ug", x)
  }
  eval(parse(text=paste0("expression(","'", x,"'",")")))
}

# Make legend
# Horizontal
{pdf("../Figures/Final/Main/scatterplot_legend_1.pdf", width=4, height=4)
  par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
  legend("top",
         legend=c("Sociodemographic", mapply(foo, plot_annot$label_ref[1:18], 1:18)),
         bty="n",text.col = c("black",darken(rep(mycolours[1],18),0.5)), cex=0.7)
  dev.off()
}

{pdf("../Figures/Main/scatterplot_legend_2.pdf", width=5, height=4.5)
  par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
  legend("top",
         legend=c("Health risk",mapply(foo, plot_annot$label_ref[19:38], 19:38)),
         bty="n",text.col = c("black",darken(rep(mycolours[2],20),0.5)), cex=0.7)
  dev.off()
}

{pdf("../Figures/Main/scatterplot_legend_3.pdf", width=3, height=4.5)
  par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
  legend("top",
         legend=c("Environmental", mapply(foo, plot_annot$label_ref[39:46], 39:46),
                  "Medical",mapply(foo, plot_annot$label_ref[47:62], 47:62)),
         bty="n",text.col = c("black",darken(rep(mycolours[3],time=8),0.5),
                              "black",darken(rep(mycolours[4],time=16),0.5)), cex=0.7)
  dev.off()
}

{pdf("../Figures/Main/scatterplot_legend_4.pdf", width=3, height=4.5)
  par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,0.2), ylim=c(0,1))
  legend("top",
         legend=c("Biomarker",mapply(foo, plot_annot$label_ref[63:90], 63:90)),
         bty="n",text.col = c("black",darken(rep(mycolours[5],28),0.2)), cex=0.7)
  dev.off()
}

# Vertical
{pdf("../Figures/Final/Main/scatterplot_legend_v.pdf", width=5, height=13.5)
  par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("top",
         legend=c("Sociodemographic", mapply(foo, plot_annot$label_ref[1:18], 1:18),
                  "Health risk",mapply(foo, plot_annot$label_ref[19:38], 19:38),
                  "Environmental", mapply(foo, plot_annot$label_ref[39:46], 39:46),
                  "Medical",mapply(foo, plot_annot$label_ref[47:62], 47:62),
                  "Biomarker",mapply(foo, plot_annot$label_ref[63:90], 63:90)),
         bty="n",text.col = c("black",darken(rep(mycolours[1],18),0.5),
                              "black",darken(rep(mycolours[2],20),0.5),
                              "black",darken(rep(mycolours[3],time=8),0.5),
                              "black",darken(rep(mycolours[4],time=16),0.5),
                              "black",darken(rep(mycolours[5],28),0.5)), cex=0.7)
  dev.off()
}
