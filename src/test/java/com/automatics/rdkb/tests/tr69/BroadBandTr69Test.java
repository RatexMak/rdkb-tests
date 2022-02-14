/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.tr69;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.TR69ParamDataType;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.TR69ParamConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;

/**
 * Test class for validating TR-69 ACS related functionality.
 * 
 * @author Selvaraj Mariyappan
 *
 */
public class BroadBandTr69Test extends AutomaticsTestBase {
    /** Constant holds the test step number **/
    private static int stepNumber = 0;

    /**
     * Test to Verify the Manufacturer serial number using TR69 parameter - Device.DeviceInfo.SerialNumber
     * 
     * @param device
     *            The device to be used.
     * 
     * @author Selvaraj Mariyappan
     * @refactor Rakesh C N
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TR69-1000")
    public void testVerifyDeviceSerialNumberUsingTR69Parameter(Dut device) {

	String testCaseId = "TC-RDKB-TR69-001";
	String stepNumber = "s1";
	boolean status = false;
	String message = null;
	String tr69InitialStatus = null;

	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("PRE-CONDITION STEPS");
	tr69InitialStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP);
	LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");
	
	/**
	 * Step 1 : Retrieve the Serial number using TR-69 parameter Device.DeviceInfo.SerialNumber and verify with the
	 * actual serial number of the device using device object
	 */

	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP 1: Retrieve the Serial number  using TR-69 parameter 'Device.DeviceInfo.SerialNumber' "
		+ "and verify with the actual serial number of the device using device object");
	LOGGER.info("EXPECTED: Device Serial number should be retrieved successfully");
	LOGGER.info("**********************************************************************************");

	try {

	    // Retrieve the serial number from the response
	    String tr69DeviceSerialNumber = BroadBandTr69Utils.executeTr69ParameterCommand(tapEnv, device,
		    TR69ParamConstants.TR69_PARAM_SERIAL_NUMBER);

	    String manufacturerSerialNumber = device.getSerialNumber();
	    message = " EXPECTED : Manufacturer Serial Number  : " + manufacturerSerialNumber;
	    LOGGER.info("ACTUAL : Device Serial Number using TR-69 ACS : " + tr69DeviceSerialNumber + " , " + message);
	    if (CommonMethods.isNotNull(tr69DeviceSerialNumber) && CommonMethods.isNotNull(manufacturerSerialNumber)) {
		// Verify with the actual serial number of the device
		status = (tr69DeviceSerialNumber.trim()).equalsIgnoreCase(manufacturerSerialNumber.trim());
		if (!status) {
		    message = "Seems like 'Device.DeviceInfo.SerialNumber' providing wrong serial number . ACTUAL : Manufacture Serial number :  "
			    + tr69DeviceSerialNumber + message;
		    LOGGER.error(message);
		}
	    } else {
		status = false;
		message = "Unable to retrieve manufacturer serial number using TR69 parameter -'Device.DeviceInfo.SerialNumber'";
		LOGGER.error(message);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);
	} catch (Exception exception) {
	    status = false;
	    message = "Unable to retrieve using TR69 parameter -'Device.DeviceInfo.SerialNumber'"
		    + exception.getMessage();
	    LOGGER.error(message);
	} finally {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION : Revert the Inital TR069 Status");
	    LOGGER.info("POST-CONDITION 1: ACTION : set Device.ManagementServer.EnableCWMP to Intial Status.");
	    LOGGER.info("POST-CONDITION 1: EXPECTED : Reverting back the Tr069 to intial status should be successful.");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP,
		    BroadBandTestConstants.CONSTANT_3, tr69InitialStatus, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"POST-CONDITION 1: ACTUAL : Reverting back the Tr069 to intial status should be successful.");
	    } else {
		LOGGER.error(
			"POST-CONDITION 1: ACTUAL : Post condition failed : Reverting back the Tr069 to intial status is not successful.");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
    }
    /**
     * Test to Verify the device model name using TR69 parameter - Device.DeviceInfo.ModelName
     * 
     * @param device
     *            The device to be used.
     * 
     * @author Selvaraj Mariyappan
     * @refactor Rakesh C N
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
   	@TestDetails(testUID = "TC-RDKB-TR69-1001")
    public void testVerifyDeviceModelNameUsingTR69Parameter(Dut device) {

	String testCaseId = "TC-RDKB-TR69-002";
	String stepNumber = "s1";
	boolean status = false;
	String message = null;

	/**
	 * Step 1 : Retrieve the Device Model Name using TR-69 parameter Device.DeviceInfo.ModelName and verify with the
	 * actual model name of the device using device object
	 */

	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP 1: Retrieve the device model name  using TR-69 parameter 'Device.DeviceInfo.ModelName' "
		+ "and verify with the actual model name of the device using device object");
	LOGGER.info("EXPECTED: Device model name should be retrieved successfully");
	LOGGER.info("**********************************************************************************");

	try {

	    // Retrieve the device model name
	    String tr69DeviceModelName = BroadBandTr69Utils.executeTr69ParameterCommand(tapEnv, device,
		    TR69ParamConstants.TR69_PARAM_MODEL_NAME);
	    String deviceModelName = device.getModel();
	    message = " EXPECTED: Device model name  : " + deviceModelName;
	    LOGGER.info("ACTUAL : Device model name using TR-69 ACS : " + tr69DeviceModelName + " , " + message);
	    if (CommonMethods.isNotNull(tr69DeviceModelName) && CommonMethods.isNotNull(deviceModelName)) {
		// Verify with the actual model name of the device
		status = (tr69DeviceModelName.trim()).equalsIgnoreCase(deviceModelName.trim());
		if (!status) {
		    message = "Seems like 'Device.DeviceInfo.ModelName' providing wrong model name . ACTUAL : Manufacture Device model name :  "
			    + tr69DeviceModelName + message;
		    LOGGER.error(message);
		}
	    } else {
		status = false;
		message = "Unable to retrieve the device model name using TR69 parameter -'Device.DeviceInfo.ModelName'";
		LOGGER.error(message);
	    }
	} catch (Exception exception) {
	    status = false;
	    message = "Unable to retrieve using TR69 parameter -'Device.DeviceInfo.ModelName'" + exception.getMessage();
	    LOGGER.error(message);
	} finally {

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);
	}
    }
    /**
     * Test to Verify the current firmware version using TR69 parameter - Device.DeviceInfo.SoftwareVersion
     * 
     * @param device
     *            The device to be used.
     * 
     * @author Selvaraj Mariyappan
     * @refactor Rakesh C N
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TR69-1002")
    public void testVerifyDeviceSoftwareVersionUsingTR69Parameter(Dut device) {

	String testCaseId = "TC-RDKB-TR69-003";
	String stepNumber = "s1";
	boolean status = false;
	String message = null;

	/**
	 * Step 1 : Retrieve the Currently running firmware version using TR-69 parameter
	 * Device.DeviceInfo.SoftwareVersion and verify with the actual currently running firmware version of the device
	 * using device object
	 */

	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 1: Retrieve the Currently running firmware version  using TR-69 parameter 'Device.DeviceInfo.SoftwareVersion' "
			+ "and verify with the actual firmware version of the device using device object");
	LOGGER.info("EXPECTED: Current software version should be retrieved successfully");
	LOGGER.info("**********************************************************************************");

	try {

	    // Retrieve the current firmware version
	    String tr69CurrentFirmwareVersion = BroadBandTr69Utils.executeTr69ParameterCommand(tapEnv, device,
		    TR69ParamConstants.TR69_PARAM_SOFTWARE_VERSION);
	    String firmwareVersion = BroadBandCommonUtils.getCurrentlyRunningImageVersionUsingWebPaCommand(tapEnv, device);
	    message = " EXPECTED : Firmware version  : " + firmwareVersion;
	    LOGGER.info("ACTUAL : Software version using TR-69 ACS : " + tr69CurrentFirmwareVersion + " , " + message);
	    if (CommonMethods.isNotNull(tr69CurrentFirmwareVersion) && CommonMethods.isNotNull(firmwareVersion)) {
		// Verify with the actual firmware version of the device
		status = CommonMethods.patternMatcher(tr69CurrentFirmwareVersion, firmwareVersion);
		if (!status) {
		    message = "Seems like 'Device.DeviceInfo.SoftwareVersion' providing wrong firmware version . ACTUAL :  Firmware version:  "
			    + tr69CurrentFirmwareVersion + message;
		    LOGGER.error(message);
		}
	    } else {
		status = false;
		message = "Unable to retrieve the Currently running firmware version using TR69 parameter -'Device.DeviceInfo.SoftwareVersion'";
		LOGGER.error(message);
	    }
	} catch (Exception exception) {
	    status = false;
	    message = "Unable to retrieve the Currently running firmware version using TR69 parameter -'Device.DeviceInfo.SoftwareVersion'"
		    + exception.getMessage();
	    LOGGER.error(message);
	} finally {

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);
	}
    }
}