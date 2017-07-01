import { ErrorHandler, Injectable} from '@angular/core';

// source: https://medium.com/@amcdnl/global-error-handling-with-angular2-6b992bdfb59c

@Injectable()
export class GlobalErrorHandler implements ErrorHandler {
  constructor() { }

  handleError(error) {
     console.log("GLOBAL error handler: ");
     console.log(error);
     
     const r:Response = error.rejection as Response;
     if (r && r.text)
      console.log(r.text()); // if the error is a http error, get the error body text

     // IMPORTANT: Rethrow the error otherwise it gets swallowed
     throw error;
  }
  
}