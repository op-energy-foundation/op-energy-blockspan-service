import { Component, OnInit } from '@angular/core';
import { lastValueFrom } from 'rxjs';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { BackendGitHash } from '../../interfaces/oe-energy.interface';

@Component({
  selector: 'app-about',
  templateUrl: './about.component.html',
  styleUrls: ['./about.component.scss'],
})
export class AboutComponent implements OnInit {
  frontendGitCommitHash: string;

  constructor(public oeEnergyApiService: OeEnergyApiService) {}

  ngOnInit() {
    this.getCommitId();
  }

  async getCommitId() {
    try {
      const data = (await lastValueFrom(this.oeEnergyApiService.$getGitHash(), {
        defaultValue: [],
      })) as BackendGitHash;
      this.frontendGitCommitHash = data.gitCommitHash;
    } catch (error) {}
  }
}
