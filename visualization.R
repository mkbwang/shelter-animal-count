

colors = c("#000080", "#008080", "#014421", "#301934",
           "#36454F", " #2F4F4F", "#4B0082", "#800000", "#556B2F",
           "#5C4033")

save_heatmap <- function(X, entry_name, title_name, rowannot=NULL, colannot=NULL,
                         cmap, na_col="#555555", border_col="black",
                         legend=F){

  # svg(filename,
  #     width=width,
  #     height=height)


  ht <- Heatmap(X, name=entry_name, cluster_rows=FALSE, cluster_columns=FALSE,
                show_column_names = TRUE, show_row_names = TRUE,
                show_heatmap_legend = legend,
                top_annotation = colannot, # Add column annotation
                left_annotation = rowannot, # Add row annotation
                row_names_side = "left",
                column_names_side = "bottom",
                column_names_rot = 0,
                column_title = title_name,
                column_title_gp = gpar(fontface = "bold", fontsize=14),
                column_names_gp = gpar(just = c(2, 2)),
                border_gp = gpar(col=border_col, lwd=0.8),
                rect_gp = gpar(col=border_col, lwd = 0.8),
                na_col = na_col,
                col = cmap)
  # Draw heatmap with minimal margin
  # draw(ht,newpage = TRUE)
  # dev.off()

  return(ht)

}


# ggplot(counts_subset_by_year, aes(x=year_of_record, y=gross_intakes, group=location_id)) +
#   geom_point(aes(color=Type), size=1.2, alpha=0.5) + geom_line(aes(color=Type, linetype=Gov), size=1, alpha=0.5) +
#   xlab("Year") + ylab("Total Number of Annual Intake") +
#   scale_x_continuous(breaks=seq(2021, 2024))+
#   scale_y_log10(breaks=c(seq(300, 1000, 100), seq(1000,5000, 500)))+
#   scale_color_manual(values = c("Shelter" = "#000080", "Rescue" = "#800000"), name = "Organization Type")+
#   scale_linetype_manual(values = c("Yes" = "solid", "No" = "dotdash"), name = "Government Affiliated")+
#   theme(
#     legend.position = "bottom",
#     legend.justification = "top"
#   )


