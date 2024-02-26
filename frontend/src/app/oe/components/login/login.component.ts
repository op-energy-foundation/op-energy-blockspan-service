import { Component, OnInit } from '@angular/core';
import { OeAccountApiService } from '../../services/oe-energy.service';
import { ActivatedRoute, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss'],
})
export class LoginComponent implements OnInit {
  isLoading: boolean = true;

  constructor(
    private oeAccountApiService: OeAccountApiService,
    private route: ActivatedRoute,
    private router: Router,
    private toastr: ToastrService
  ) {}

  ngOnInit() {
    this.route.paramMap.subscribe((params) => {
      const secret = params.get('secret');
      if (secret) {
        this.performLogin(secret);
      } else if (!this.oeAccountApiService.$tokenExists()) {
        this.performRegistration();
      } else {
        this.isLoading = false;
        this.router.navigate(['/preview-page']);
      }
    });
  }

  performLogin(secret: string) {
    this.oeAccountApiService.$login(secret).subscribe({
      next: (accountToken) => {
        this.oeAccountApiService.$saveToken(accountToken);
        this.isLoading = false;
        this.router.navigate(['/preview-page']);
      },
      error: (error) => {
        this.isLoading = false;
        this.oeAccountApiService.$cleanToken();
        this.toastr.error(`Login failed: ${error.error}`, 'Failed!');
        this.router.navigate(['/preview-page']);
      },
    });
  }

  performRegistration() {
    this.oeAccountApiService.$register().subscribe({
      next: (data) => {
        this.oeAccountApiService.$saveToken(data.accountToken);
        this.isLoading = false;
        this.router.navigate(['/login', data.accountSecret]);
      },
      error: (error) => {
        this.isLoading = false;
        this.oeAccountApiService.$cleanToken();
        this.toastr.error(`Registration failed: ${error.error}`, 'Failed!');
        this.router.navigate(['/preview-page']);
      },
    });
  }
}
