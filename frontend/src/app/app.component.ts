import { Component } from '@angular/core';
import { WebsocketService } from './oe/services/websocket.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent {
  constructor(private webSocket: WebsocketService) {}
}
