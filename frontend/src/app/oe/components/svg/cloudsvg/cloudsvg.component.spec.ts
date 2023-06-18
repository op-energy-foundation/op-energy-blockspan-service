import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CloudsvgComponent } from './cloudsvg.component';

describe('CloudsvgComponent', () => {
  let component: CloudsvgComponent;
  let fixture: ComponentFixture<CloudsvgComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CloudsvgComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CloudsvgComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
