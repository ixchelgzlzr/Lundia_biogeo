source("R_scripts/plot_anc_range.util.R")

library(RevGadgets)

# change base according to tree
base = "output/ML_MCC/ML_MCC" 
#base = "output/MCC_MCC/MCC_MCC" 


# build # file names
plot_fn = paste0(base, "_DEC_range.pdf")
tree_fn = paste0(base, ".ase.tre")
label_fn = paste0(base, ".state_labels.txt")
color_fn = "data/range_color.txt"

# get state labels and state colors
states = make_states(label_fn, color_fn)
state_labels = states$state_labels
state_colors = states$state_colors

# process the ancestral states
ase <- processAncStates(tree_fn,
                        # Specify state labels.
                        # These numbers correspond to
                        # your input data file.
                        state_labels = state_labels)

# plot the ancestral states

pp  <- plotAncStatesPie(t = ase,
                         tip_labels_size=3.5,
                         tip_labels_offset=0.5,
                         node_labels_size=1,
                         tip_pie_size=1.2,
                         node_pie_size=1.5,
                         pie_colors = state_colors,
                         timeline = T,
                         tip_labels_italics = T,
                         state_transparency = 1
                        ) +
  # Move the legend
  theme(legend.position = c(0.1, 0.75))


pp
# get plot dimensions
#x_phy = max(pp$data$x)       # get height of tree
#x_label = 3.5                # choose space for tip labels
#x_start = 1                  # choose starting age (greater than x_phy)
#x0 = -(x_start - x_phy)      # determine starting pos for xlim
#x1 = x_phy + x_label         # determine ending pos for xlim

# add axis
pp = pp + theme_tree2()
pp = pp + labs(x="Age (Ma)")

# change x coordinates
#pp = pp + coord_cartesian(xlim=c(x0,x1), expand=TRUE)

# plot axis ticks
#island_axis = sec_axis(~ ., breaks=x_phy-c(5.1, 2.95, 1.55, 0.5), labels=c("+K","+O","+M","+H") )
#x_breaks = seq(0,x_start,1) + x0
#x_labels = rev(seq(0,x_start,1))
#pp = pp + scale_x_continuous(breaks=x_breaks, labels=x_labels, sec.axis=island_axis)

# plot island age intervals
#pp = add_island_times(pp, x_phy)

# set up the legend
#pp = pp + guides(color = guide_legend(override.aes = list(size=5), ncol=2))
pp = pp + theme(legend.position="right")

pp

# save
ggsave(file=plot_fn, plot=pp, device="pdf", height=7, width=10, useDingbats=F)

