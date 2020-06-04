function logLike=mcmcLikelihood(modelOut,data)
  
  [row,col]=size(data);
  logLike=0;		
  
  for i=1:row		%%% Loop through the rows
       index=find(data(i,:)~=-9999);
       nValid=length(index);
  	
    	sq = sum( (modelOut(i,index) - data(i,index)).^2 );
    	sigma = sqrt(sq/nValid);
    
    	logLike= logLike+sq/(2*sigma^2);
    	logLike= logLike+nValid*log(sigma);
    	logLike= logLike+nValid*sqrt(2*pi);
  
  end
  	
 