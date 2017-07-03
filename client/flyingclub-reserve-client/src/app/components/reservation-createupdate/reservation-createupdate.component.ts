import { Component, OnInit } from '@angular/core';
import { Reservation } from 'app/models/Reservation';
import { Airplane } from 'app/models/airplane';

import { AirplaneService } from 'app/services/airplane.service';

@Component({
  selector: 'reservation-createupdate',
  templateUrl: './reservation-createupdate.component.html',
  styleUrls: ['./reservation-createupdate.component.css']
})
export class ReservationCreateupdateComponent implements OnInit {
  model:Reservation = new Reservation();
  planes:Airplane[] = [];


  constructor(private airplane: AirplaneService) { }

  async ngOnInit() {
    const p = await this.airplane.airplanes();
    this.planes = p;
    console.log(1);
  }


  async createOrUpdateReservation() {        
    
     console.log(this.model);


  }

}
