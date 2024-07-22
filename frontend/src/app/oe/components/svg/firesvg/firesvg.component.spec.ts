import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FiresvgComponent } from './firesvg.component';

describe('FiresvgComponent', () => {
  let component: FiresvgComponent;
  let fixture: ComponentFixture<FiresvgComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ FiresvgComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(FiresvgComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
