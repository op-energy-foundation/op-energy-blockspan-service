import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GussingGameComponent } from './guessing-game.component';

describe('GussingGameComponent', () => {
  let component: GussingGameComponent;
  let fixture: ComponentFixture<GussingGameComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GussingGameComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(GussingGameComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
