# multi-spatial-dashboard

The SenseBox is an arduino-based system to plug in different sensors in order to con-
struct an environmenmtal-measurement-station on your own and upload the quanti-
fied data to the OpenSenseMap. The foundation of being able to develop a dashboard
built on SenseBox measurements is the aspect of non-restricted accesibility to data
in the context of citizen-science. The data can be retrieved with the opensensemap
API.
The platform to be devoloped shall enable the user to create a multi-spatial dash-
board with a variety of options in matter of statistical-visualisations.
My motivation is to improve the people’s comprehension about the correlation
between environmental phenomena by creating a platform where the quantified data
can be easily transformed into qualified visualisations by any user. The operations
that can be applied to the data will include minimum, maximum, arithmetic mean,
standard deviation, variance and trends. In addition to that, the user will be pro-
vided with average values according to their recent.
The goals can be segmented into a technical and a science-communicational part.
The technical part is based on the successful implementation and the development
of an intuitive operational dashboard. To improve the user’s understanding there
will be given additional textual information to the graphics. In the context of open-
source, the more interested user will have the chance of an insight to the code that
created the platform.
Adressing a diverse group of users is accomponied by defining the spatial areas
according to their administrative borders. Thus the field of users can be seperated
into citizens and advanced users. A citizen-user can be of any age class of any scope.
An advanced user as well can further be devided for example into one that is more
interested in the implementation and one who’s involved in the adminstration of an
area.
Also, the dashboard will include a feature that shows fun-facts like ”oldest registred
SenseBox”, ”SenseBox with the most sensors”, etc.
Implementing the system I will use the statistic software R.
Corresponding to R, the framework I am going to use is the R-package Shiny.
Finally, as a general technical requirement, the dashboard shall become a responsive
and performant platform to work with.