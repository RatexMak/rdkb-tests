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
package com.automatics.rdkb.tests.system;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestCategory;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandSnmpConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.interfaces.FactoryResetSettings;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.AdvancedFeatureServices;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.CodeBig;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.ForceWiFiDisable;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.LimitBeaconDetectionEnum;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.LocalGatewayIPv4;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.ParentalControlService;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.RabidMemoryLimit;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.TelemetryEndpoint;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.WifiSettings2Ghz;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.WifiSsid;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.WifiSsidSnmp;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandFactoryResetTests extends AutomaticsTestBase {
	
	
    /**
    *
    * Test Case # 1: Perform Factory Reset Using WebPA.
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>PRECONDITION 1 - Get Default SSID value for 2.4Ghz and 5 Ghz using WebPA request.</li>
    * <li>PRECONDITION 2 - Get Default value for Parental Control Services.</li>
    * <li>PRECONDITION 3 - Get Default value for Advanced feature Services.</li>
    * <li>PRECONDITION 4 -Get Default value for LOCAL Gateway IPv4, DHCP LOCAL IPv4 Start, DHCP LOCAL IPv4 End</li>
    * <li>PRECONDITION 5 -Set Default value for 2.4GHz Channel bandwidth as 40 MHz</li>
    * <li>Step 1 - Verify setting SSID Name value other than default SSID Name of 2.4Ghz and 5 Ghz using WebPA request.
    * </li>
    * <li>Step 2 -Modify Parental Control service value other than default value.</li>
    * <li>Step 3 -Modify Advanced feature service value other than default value.</li>
    * <li>Step 4 -Modify Local Gateway IPV4, DHCP start and end value.</li>
    * <li>Step 5 -Verify Changing 2.4 GHz channel bandwidth to value other than 20/40 MHz.</li>
    * <li>Step 6 -Verify SSID value of 2.4Ghz and 5 Ghz with default SSID value after Factory Reset.</li>
    * <li>Step 7 -Verify Parental Control service value with the default value
    * <li>Step 8 -Verify Advanced feature service value with default value. .</li>
    * <li>Step 9 - Verify Local Gateway IPV4, DHCP start and end value with default value.</li>
    * <li>Step 10 : Verify the Current SSID value for 2.4 Ghz using SNMP request.</li>
    * <li>Step 11 : Verify 2.4 GHz channel bandwidth value is 20/40 MHz.
    * <li>
    * <li>Step 12 : Verify 5GHZ Xfinity Home SSID is disabled through WebPA request.
    * <li>
    * 
    */
	 
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
   @TestDetails(testUID = "TC-RDKB-FACTORYRBT-1002")
   public void testToVerifyFactoryResetUsingWebpa(Dut device) {
	String testCaseId = "TC-RDKB-FACTORYRBT-002";
	testToVerifyFactoryReset(device, testCaseId, TestCategory.WEBPA);
   }
	
    /**
    *
    * Test Case # 2: Perform Factory Reset Using SNMP.
    * 
    * <ol>
    * <li>Pre-Condition 1: Verify WAN status through sysevent.</li>
    * <li>Pre-Condition 2: Verify getting default SSID values for 2.4GHz and 5GHz using WebPA request.</li>
    * <li>Pre-Condition 3: Verify getting default SSID values for both 2.4GHz & 5GHz via SNMP.</li>
    * <li>Pre-Condition 4: Verify getting default values for Parental Control Services.</li>
    * <li>Pre-Condition 5: Verify getting default values for Advanced feature Services.</li>
    * <li>Pre-Condition 6: Verify getting default values for LOCAL Gateway IPv4, DHCP LOCAL IPv4 Start, DHCP LOCAL IPv4
    * End.</li>
    * <li>Pre-Condition 7: Verify getting default value for 2.4GHz Channel bandwidth.</li>
    * <li>Pre-Condition 8: Verify CodeBig Enable is disabled by default.</li>
    * <li>Pre-Condition 9: Verify Telemetry Endpoint is disabled by default.</li>
    * <li>Pre-Condition 10: Verify getting default value for LimitBeaconDetection.</li>
    * <li>Pre-Condition 11: Verify getting default value for rabid framework memory limit.</li>
    * <li>Pre-Condition 12: Verify force wifi disable is disabled by default.</li>
    * <li>Verify device uptime is more than 20 minutes.</li>
    * <li>Verify setting SSID Name value other than default SSID Name of 2.4Ghz and 5 Ghz using WebPA request.</li>
    * <li>Verify modifying Parental Control service value other than default value.</li>
    * <li>Verify Modifying Advanced feature service value other than default value.</li>
    * <li>Verfiy Modifying Local Gateway IPV4, DHCP start and end value other than default value.</li>
    * <li>Verify Changing 2.4 GHz channel bandwidth to value other than 20/40 MHz.</li>
    * <li>Verify Enabling Telemetry Endpoint through WebPA request.</li>
    * <li>Verify Setting LimitBeaconDetection to true.</li>
    * <li>Verify Changing rabid framework memory limit to 10MB.</li>
    * <li>Verify setting Force WiFi Disable to true.</li>
    * <li>Verify Performing Factory reset on the device using SNMP request.</li>
    * <li>Verify the Current SSID value for 2.4Ghz and 5 Ghz using WebPA request.</li>
    * <li>Verify Parental Control service value with the default value.</li>
    * <li>Verify Advanced feature service value with default value.</li>
    * <li>Verify Local Gateway IPV4, DHCP start and end value with default value.</li>
    * <li>Verify the Current SSID value for 2.4Ghz and 5 Ghz using SNMP request.</li>
    * <li>Verify 2.4 GHz channel bandwidth value is 20/40 MHz.</li>
    * <li>Verify that the CodeBigFirst Enable has default value.</li>
    * <li>Verify that the TelemetryEndpoint Enable has default value.</li>
    * <li>Verify the default value of LimitBeaconDetection is false after Factory Reset.</li>
    * <li>Verify that the Rabid framework memory limit has default value.</li>
    * <li>Verify that Force WiFi Disable has default value.</li>
    * 
    */
   @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
   @TestDetails(testUID = "TC-RDKB-FACTORYRBT-1003")
   public void testToVerifyFactoryResetUsingSnmp(Dut device) {
	// Test case id declaration
	String testCaseId = "TC-RDKB-FACTORYRBT-003";
	testToVerifyFactoryReset(device, testCaseId, TestCategory.SNMP);
   }
	
	
    /**
     * Test method used to get and verify the Wan IP and Client IP
     * 
     * @param device
     *            Instance of {@link Dut}
     * @param testId
     *            Test case ID
     * @param stepNumber
     *            Step Number
     * @param initialCommonParamValues
     *            Key and pair value mapping
     * @param isCompare
     *            True- Compare with initial values
     * @throws TestException
     * @refactor Govardhan
     */
    public static Map<String, String> executeTestSeptsToGetAndVerifyWanIPAndClientIP(Dut device, String testId,
	    int stepNumber, Map<String, String> initialCommonParamValues, boolean isCompare) throws TestException {
	String step = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	String successMessage = null;
	Map<String, String> commonParamValues = null;
	/**
	 * Step : RETRIEVE THE WAN IP AND CLIENT IP ROUTER VALUES FROM DEVICE
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Retrieve WAN IP and Client IP Router values from device");
	LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
		+ "	1. Get the value for WAN IPv4 Address \n"
		+ "	2. Get the value for DHCP v4 client IP routers");
	LOGGER.info(
		"STEP " + stepNumber + " : EXPECTED : Must return the WAN IP and Client IP Router values from device.");
	LOGGER.info("**********************************************************************************");
	errorMessage = "Unable to retrieve WAN IP and Client IP Router values from device.";
	try {
	    commonParamValues = BroadBandWebPaUtils.executeAndGetListOfWebParameters(device, tapEnv,
		    BroadBandWebPaConstants.WEB_PARAM_FOR_WAN_CLIENT_IP);
	    status = !commonParamValues.isEmpty();
	    successMessage = "Sucessfully retrieved the WAN IP and Client IP Router values from device";

	    if (status) {
		status = false;
		if (isCompare) {
		    LOGGER.info("Comparing the WAN IP and Client IP Router with initial values");
		    errorMessage = "Unable to cross verify the WAN IP and Client IP Router values with initial WAN IP and Client IP Router values.";
		    status = BroadBandWebPaUtils.verifyWebpaGetResponseValues(device, tapEnv, initialCommonParamValues,
			    commonParamValues);
		    successMessage = "Sucessfully cross verified the WAN IP and Client IP Router values with initial WAN IP and Client IP Router values.";
		} else {
		    LOGGER.info("Verifying the WAN IP and Client IP Router are empty or null");
		    status = BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(commonParamValues);
		}
	    }
	} catch (Exception e) {
	    errorMessage += e.getMessage();
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : " + successMessage);
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	return commonParamValues;
    }

    /**
     * Test method used to set and verify the wifi radio values
     * 
     * @param device
     *            Instance of {@link Dut}
     * @param testId
     *            Test case ID
     * @param stepNumber
     *            Step Number
     * @throws TestException
     * @refactor Govardhan
     */
    public static Map<String, String> executeTestSeptsToSetTheRadioValues(Dut device, String testId, int stepNumber)
	    throws TestException {
	String step = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	Map<String, String> setValuesMap = null;
	/**
	 * Step : SET AND VERIFY THE 2.4 & 5 GHZ RADIO VALUES FROM DEVICE
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP " + stepNumber + " : DESCRIPTION : Set and verify the value for 2.4 & 5 GHz Radios in device");
	LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
		+ "1. Get the value for 2.4 GHz radio extension channel \n"
		+ "2. Get the value for 2.4 GHz radio beacon interval \n"
		+ "3. Get the value for 2.4 GHz radio basic rate \n"
		+ "4. Get the value for 2.4 GHz radio operating standard \n"
		+ "5. Get the value for 2.4 GHz radio transmit power \n"
		+ "6. Get the value for 2.4 GHz radio status \n" + "7. Get the value for 2.4 GHz radio channel \n"
		+ "8. Get the value for 2.4 GHz radio wireless channel \n"
		+ "9. Get the value for 2.4 GHz radio operating channel bandwidth \n"
		+ "10. Get the value for 2.4 GHz radio dfs enable \n"
		+ "11. Get the value for 5 GHz radio extension channel \n"
		+ "12. Get the value for 5 GHz radio beacon interval \n"
		+ "13. Get the value for 5 GHz radio basic rate \n"
		+ "14. Get the value for 5 GHz radio operating standard \n"
		+ "15. Get the value for 5 GHz radio transmit power \n" + "16. Get the value for 5 GHz radio status \n"
		+ "17. Get the value for 5 GHz radio channel \n"
		+ "18. Get the value for 5 GHz radio wireless channel \n"
		+ "19. Get the value for 5 GHz radio operating channel bandwidth \n"
		+ "20. Get the value for 5 GHz radio dfs enable ");
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must set the value for 2.4 & 5 GHz Radios in device.");
	LOGGER.info("**********************************************************************************");
	errorMessage = "Unable to set and verify the 2.4 & 5 GHz Radios values in device.";
	setValuesMap = BroadBandWebPaUtils.setTheWebParamForWifiRadios(device, tapEnv, null);
	status = (setValuesMap != null && !setValuesMap.isEmpty());
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : Successfully set and verified the value for 2.4 & 5 GHz Radios in device");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	return setValuesMap;
    }

    /**
     * Test method used to set and verify the common parameter values
     * 
     * @param device
     *            {@link Dut}
     * @param stepNumber
     *            Step Number
     * @throws TestException
     * @refactor Govardhan
     */
    public static Map<String, String> executeTestSeptsToSetTheCommonParameterValues(Dut device, String testId,
	    int stepNumber) throws TestException {
	String step = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	Map<String, String> setValuesMap = null;
	/**
	 * Step : SET AND VERIFY THE COMMON PARAMETER VALUES FROM DEVICE
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Set and verify the common parameter values in device");
	LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
		+ "	1. Set the value for DMZ LAN IP Address \n" + "	2. Set the value for Device cloud UI status \n"
		+ "	3. Set the value for device control router enable status \n"
		+ "	4. Set the value for port forwarding status");
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must set the common parameter values in device.");
	LOGGER.info("**********************************************************************************");
	errorMessage = "Unable to set and verify the common parameter values in device.";
	setValuesMap = BroadBandWebPaUtils.setTheCommonWebParams(device, tapEnv, null, false);
	status = (setValuesMap != null && !setValuesMap.isEmpty());
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : Successfully set and verified the common parameter values in device");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	return setValuesMap;
    }

    /**
     * Test method used to get and verify the wifi radio values
     * 
     * @param device
     *            {@link Dut}
     * @param testId
     *            Test case ID
     * @param stepNumber
     *            Step Number
     * @param initialWiFiRadioValues
     *            Key and pair value mapping
     * @param isCompare
     *            True- Compare with initial values
     * @throws TestException
     * @refactor Govardhan
     */
    public static void executeTestSeptsToGetAndVerifyRadioValues(Dut device, String testId, int stepNumber,
	    Map<String, String> initialWiFiRadioValues, boolean isCompare) throws TestException {
	String step = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	String successMessage = null;
	Map<String, String> wifiRadioValues = null;
	/**
	 * Step : RETRIEVE THE 2.4 & 5 GHZ RADIO VALUES FROM DEVICE
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Retrieve value for 2.4 & 5 GHz Radios from device");
	LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
		+ "1. Get the value for 2.4 GHz radio extension channel \n"
		+ "2. Get the value for 2.4 GHz radio beacon interval \n"
		+ "3. Get the value for 2.4 GHz radio basic rate \n"
		+ "4. Get the value for 2.4 GHz radio operating standard \n"
		+ "5. Get the value for 2.4 GHz radio transmit power \n"
		+ "6. Get the value for 2.4 GHz radio status \n" + "7. Get the value for 2.4 GHz radio channel \n"
		+ "8. Get the value for 2.4 GHz radio wireless channel \n"
		+ "9. Get the value for 2.4 GHz radio operating channel bandwidth \n"
		+ "10. Get the value for 2.4 GHz radio dfs enable \n"
		+ "11. Get the value for 5 GHz radio extension channel \n"
		+ "12. Get the value for 5 GHz radio beacon interval \n"
		+ "13. Get the value for 5 GHz radio basic rate \n"
		+ "14. Get the value for 5 GHz radio operating standard \n"
		+ "15. Get the value for 5 GHz radio transmit power \n" + "16. Get the value for 5 GHz radio status \n"
		+ "17. Get the value for 5 GHz radio channel \n"
		+ "18. Get the value for 5 GHz radio wireless channel \n"
		+ "19. Get the value for 5 GHz radio operating channel bandwidth \n"
		+ "20. Get the value for 5 GHz radio dfs enable ");
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must return the 2.4 & 5 GHz Radios values from device.");
	LOGGER.info("**********************************************************************************");
	errorMessage = "Unable to retrieve the 2.4 & 5 GHz Radios values from device.";
	try {
	    wifiRadioValues = BroadBandWebPaUtils.executeAndGetListOfWebParameters(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMS_FOR_BOTH_RADIOS);
	    status = !wifiRadioValues.isEmpty();
	    successMessage = "Sucessfully retrieved the 2.4 & 5 GHz Radios values from device";
	    if (status) {
		status = false;
		if (isCompare) {
		    LOGGER.info("Comparing the wifiRadioValues with initial values");
		    errorMessage = "Unable to cross verify the 2.4 & 5 GHz Radios values with 2.4 & 5 GHz Radios values.";
		    status = BroadBandWebPaUtils.verifyWebpaGetResponseValues(device, tapEnv, initialWiFiRadioValues,
			    wifiRadioValues);
		    successMessage = "Sucessfully cross verified the 2.4 & 5 GHz Radios values with initial 2.4 & 5 GHz Radios values.";
		} else {
		    LOGGER.info("Verifying the wifiRadioValues are empty or null");
		    status = BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(wifiRadioValues);
		}
	    }
	} catch (Exception e) {
	    errorMessage += e.getMessage();
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : " + successMessage);
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
    }

    /**
     * Test method used to get and verify the common parameter values
     * 
     * @param device
     *            Instance of {@link Dut}
     * @param testId
     *            Test case ID
     * @param stepNumber
     *            Step Number
     * @param initialCommonParamValues
     *            Key and pair value mapping
     * @param isCompare
     *            True- Compare with initial values
     * @throws TestException
     * @refactor Govardhan
     */
    public static void executeTestSeptsToGetAndVerifyCommonParameterValues(Dut device, String testId, int stepNumber,
	    Map<String, String> initialCommonParamValues, boolean isCompare) throws TestException {
	String step = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	String successMessage = null;
	Map<String, String> commonParamValues = null;
	/**
	 * Step : RETRIEVE THE COMMON PARAMETER VALUES FROM DEVICE
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Retrieve common parameter values from device");
	LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
		+ "	1. Get the value for DMZ LAN IP Address \n" + "	2. Get the value for Device cloud UI status \n"
		+ "	3. Get the value for device control router enable status \n"
		+ "	4. Get the value for port forwarding status");
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must return the common parameter values from device.");
	LOGGER.info("**********************************************************************************");
	errorMessage = "Unable to retrieve the common parameter values from device.";
	try {
	    commonParamValues = BroadBandWebPaUtils.executeAndGetListOfWebParameters(device, tapEnv,
		    DeviceModeHandler.isBusinessClassDevice(device)
		    ? BroadBandWebPaConstants.WEBPA_PARAMS_FOR_COMMON_BUSI_DEVICE
		    : BroadBandWebPaConstants.WEBPA_PARAMS_FOR_COMMON_RESI_DEVICE);
	    status = !commonParamValues.isEmpty();
	    successMessage = "Sucessfully retrieved the common parameter values from device";
	    if (status) {
		status = false;
		if (isCompare) {
		    LOGGER.info("Comparing the commonParamValues with initial values");
		    errorMessage = "Unable to cross verify the common parameter values with initial common parameter values.";
		    status = BroadBandWebPaUtils.verifyWebpaGetResponseValues(device, tapEnv, initialCommonParamValues,
			    commonParamValues);
		    successMessage = "Sucessfully cross verified the common parameter values with initial common parameter values.";
		} else {
		    LOGGER.info("Verifying the commonParamValues are empty or null");
		    status = BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(commonParamValues);
		}
	    }
	} catch (Exception e) {
	    errorMessage += e.getMessage();
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : " + successMessage);
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
    }

    /**
     * Test method used to enable the bridge mode
     * 
     * @param settop
     *            Instance of {@link Settop}
     * @param testId
     *            Test case ID
     * @param stepNumber
     *            Step Number
     * @throws TestException
     * @refactor Govardhan
     */
    public static boolean executeTestSeptsToEnableBridgeMode(Dut device, String testId, int stepNumber)
	    throws TestException {
	String step = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Enable Bridge mode in device");
	LOGGER.info("STEP " + stepNumber + " : ACTION : Execute Command: "
		+ BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE);
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : Device should return the current mode and the value should be \"bridge-static\"");
	LOGGER.info("**********************************************************************************");
	try {
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.LAN_MANAGEMENT_MODE_BRIDGE_STATIC,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		status = false;
		LOGGER.info("Mode changed to " + BroadBandTestConstants.LAN_MANAGEMENT_MODE_BRIDGE_STATIC);
		LOGGER.info("Waiting for three minutes");
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    }
	} catch (Exception e) {
	    errorMessage += e.getMessage();
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully verified bridge mode status using webpa");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
	return status;
    }
    
	/**
     * Method to verify factory reset via different method (Webpa, TR69 & SNMP)
     * 
     * @param device
     *            Dut instance
     * @param testCaseId
     *            testcase Id
     * @param factoryResetType
     *            Factory reset method to be done through WebPA/TR69/SNMP
     */
    public void testToVerifyFactoryReset(Dut device, String testCaseId, String factoryResetType) {
	// Variable declaration starts
	int stepNum = 1;
	int preConditionNumber = 0;
	int uptimeInMin = 0;
	String stepNumber = "s" + stepNum;
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// Variable declaration ends
	try {
		
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: " + testCaseId);
	    LOGGER.info("TEST DESCRIPTION: Verifyy performing Factory Reset Using " + factoryResetType);
	    LOGGER.info("#######################################################################################");

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    preConditionNumber++;
	    
	    try {
		errorMessage = "Unable to execute sysevent getwan-status command.";
		LOGGER.info("#######################################################################################");
		LOGGER.info(
			"PRE-CONDITION " + preConditionNumber + " : DESCRIPTION : Verify WAN status through sysevent.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
			+ ": ACTION : SSH the device & Execute the following command immediately after reboot: \\n\" + \"sysevent getwan-status");
		LOGGER.info("PRE-CONDITION " + preConditionNumber + ": EXPECTED :WAN status should be retrieved.");
		LOGGER.info("#######################################################################################");
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.GET_WAN_STATUS);
		status = CommonMethods.isNotNull(response) && !response.contains(BroadBandTestConstants.CMD_NOT_FOUND);
	    } catch (Exception exception) {
		LOGGER.error("Exception occured while verifying WAN status through sysevent.");
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION " + preConditionNumber + " : ACTUAL : WAN status retrieved as follows- "
			+ response);
	    } else {
		LOGGER.error("PRE-CONDITION " + preConditionNumber + " : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + preConditionNumber + ": FAILED : " + errorMessage);
	    }
	    

	    // Map containing default values for factory reset setting option.
	    Map<FactoryResetSettings, String> defaultValues = getDefaultValuesInMap(device, preConditionNumber);
	    // List containing factory reset settings option for which value is
	    // not successfully.
	    List<FactoryResetSettings> skipList = new ArrayList<FactoryResetSettings>();
	    
	    // Adding Parameter to Skip list if unable to get default value for
	    // that parameter
	    for (Map.Entry<FactoryResetSettings, String> entry : defaultValues.entrySet()) {
		if (CommonMethods.isNull(entry.getValue())
			|| CommonMethods.patternMatcher((entry.getValue()),
				BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
			|| CommonMethods.patternMatcher((entry.getValue()),
				BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
			|| CommonMethods.patternMatcher((entry.getValue()),
				BroadBandSnmpConstants.SNMP_ERROR_NON_WRITABLE)) {
		    skipList.add(entry.getKey());
		    LOGGER.info("Default Value is not obtained for parameter " + entry.getKey()
			    + " so adding parameter to skip list.");

		}
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /** Step 1: Verifying whether device uptime is more than 20 min*/
	    errorMessage = "Unable to get device Uptime.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify device uptime is more than 20 minutes.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute the Webpa Get command for param: Device.DeviceInfo.UpTime");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Uptime should be more than 20 minutes.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_UPTIME);
	    if (CommonMethods.isNotNull(response)) {
		uptimeInMin = Integer.parseInt(response) / BroadBandTestConstants.CONSTANT_60;
		if (uptimeInMin > BroadBandTestConstants.CONSTANT_20) {
		    LOGGER.info("Device uptime is more than 20 min.");
		} else {
		    int WaitTime = BroadBandTestConstants.CONSTANT_20 - uptimeInMin;
		    LOGGER.error("Device uptime is " + uptimeInMin + ". Will wait for " + WaitTime + " min.");
		    tapEnv.waitTill(WaitTime * 60000);
		}
		status = true;
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Device uptime should be more than 20 minutes.");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    
	    /**
	     * Step 2 : Verify setting SSID Name value other than default SSID Name of 2.4Ghz and 5 Ghz using WebPA
	     * request.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify setting SSID Name value other than default SSID Name of 2.4Ghz and 5 Ghz using WebPA request.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION :Execute WebPA set command  for following param: 'Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID' & 'Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID'.");
	    LOGGER.info("STEP " + stepNumber + " : WebPa Set Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    FactoryResetSettings[] factoryResetSettings = WifiSsid.values();
	    errorMessage =BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified setting SSID name value other than default value.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 3 : Modify Parental Control service value other than default value
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify modifying Parental Control service value other than default value.");
	    LOGGER.info("STEP " + stepNumber + " : ACTION :Execute the WebPA set command for following param: "
		    + "Parameter and Value : \\n" + "1. Parental Control Managed Sites \\n"
		    + "Name : Device.X_Comcast_com_ParentalControl.ManagedSites.Enable, value: true, dataType:3 \\n"
		    + "2. Parental Control Managed Service \\n"
		    + "Name :Device.X_Comcast_com_ParentalControl.ManagedServices.Enable, value : true, dataType:3 \\n"
		    + "3. Parental Control Managed Devices \\n"
		    + "Name : Device.X_Comcast_com_ParentalControl.ManagedDevices.Enable, Value:true, dataType:3");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : Webpa Command should return success message ");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = ParentalControlService.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully modified parental control value other than default values.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 4 : Modify Advanced feature service value other than default value.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify Modifying Advanced feature service value other than default value.");
	    LOGGER.info("STEP " + stepNumber + " : ACTION :Execute the WebPA set command for following params:"
		    + "Parameter and Value : \\n" + "1. Port Mapping \\n"
		    + "Name : Device.NAT.X_Comcast_com_EnablePortMapping, value: true, dataType:3 \\n"
		    + "2. HS  Port Mapping -  \\n"
		    + "Name : Device.NAT.X_Comcast_com_EnableHSPortMapping, value : false, dataType:3 \\n"
		    + "3. Port Trigger \\n"
		    + "Name : Device.NAT.X_CISCO_COM_PortTriggers.Enable, value:true, dataType:3 \\n"
		    + "4.Remote Management \\n"
		    + "Name : Device.UserInterface.X_CISCO_COM_RemoteAccess.Enable, value : true, dataType:3 \\n"
		    + "5. DMZ \\n" + "Name : Device.NAT.X_CISCO_COM_DMZ.Enable, value:true, dataType:3 \\n"
		    + "6.Routing > Authentication Type \\n"
		    + "Name : Device.Routing.RIP.InterfaceSetting.1.X_CISCO_COM_AuthenticationType,value: \"SimplePassword\", dataType:0 \\n"
		    + "7.Dynamic DNS \\n" + "Name : Device.X_CISCO_COM_DDNS.Enable, value: true, dataType:3 \\n"
		    + "8.Device Discovery > upnp \\n" + "Name : Device.UPnP.Device.Enable, value : false, dataType:3 ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = AdvancedFeatureServices.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully modified Advanced feature service value other than default values.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 5 : Modify Local Gateway IPV4, DHCP start and end value.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verfiy Modifying Local Gateway IPV4, DHCP start and end value other than default value.");
	    LOGGER.info("STEP " + stepNumber + " : ACTION :Execute the WebPA set command for following params: "
		    + "Parameter and Value : \\n" + "1. LOCAL GATEWAY IP \\n"
		    + "Name : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanIPAddress, value: 10.0.10.1, dataType:0 \\n"
		    + "2. LOCAL DHCP start address \\n"
		    + "Name :Device.DHCPv4.Server.Pool.1.MinAddress, value : 10.0.10.12, dataType:0 \\n"
		    + "3. LOCAL DHCP end address \\n"
		    + "Name : Device.DHCPv4.Server.Pool.1.MaxAddress, Value: 10.0.10.220, dataType:0");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = LocalGatewayIPv4.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully modified local gateway IPV4,DHCP start ,DHCP end value other than default values");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 6 :Verify Changing 2.4 GHz channel bandwidth to value other than 20/40 MHz.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify Changing 2.4 GHz channel bandwidth to value other than 20/40 MHz.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION :Execute the WebPa Get command for following param: Device.WiFi.Radio.10000.OperatingChannelBandwidth and set value as 20.");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = WifiSettings2Ghz.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified changing 2.4GHz channel bandwidth to value other than default value.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 8 : Enable Code big using WebPA request.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION :  Verifying Enabling Code Big through WebPA request.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute the Webpa Set command for following param : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable and set value as 'true'.");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = CodeBig.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified changing Code Big enable to value other than default value");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 9 : Enable Telemetry Endpoint through WebPA request
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION :  Verify Enabling Telemetry Endpoint through WebPA request.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute the WebPa set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable & set value as 'true'.");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = TelemetryEndpoint.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified changing Telemetry Endpoint Enable to value other than default value.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 10 : set LimitBeaconDetectionEnum to true.
	     */

	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Setting LimitBeaconDetection to true.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the WebPa Set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection and set value as true.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    
		Boolean isSupportedDevices = null;
		
		isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

		if (isSupportedDevices)
	    {
		factoryResetSettings = LimitBeaconDetectionEnum.values();
		 errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
		status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
		if (status) {
		    LOGGER.info("STEP " + stepNumber + " : ACTUAL : LimitBeaconDetection is set to true.");
		} else {
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    /**
	     * Step 11 :Verify Changing rabid framework memory limit to 10MB.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + " : DESCRIPTION : Verify Changing rabid framework memory limit to 10MB.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION :Execute the Webpa Set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.MemoryLimit & set value as '10'.");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    //TR-181 to configure "MemoryLimit" for Rabid is not applicable for Business class devices    
	    if (DeviceModeHandler.isDSLDevice(device) || DeviceModeHandler.isBusinessClassDevice(device)){
		LOGGER.info("This Step is not applicable for DSL and Business class devices");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			"Step is not applicable for DSL and Business class devices", false);
	    } else {
		factoryResetSettings = RabidMemoryLimit.values();
		 errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
		status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
		if (status) {
		    LOGGER.info("STEP " + stepNumber
			    + " : ACTUAL : Successfully verified changing rabid framework memory limit to value other than default value");
		} else {
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    }

	    /**
	     * Step 12 : Set Force WiFi Disable to true
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify setting Force WiFi Disable to true.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute the Webpa Set command for following param: Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable & set value as 'true'.");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = ForceWiFiDisable.values();
	    errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList,tapEnv);
	    status = !CommonUtils.isGivenStringAvailableInCommandOutput(errorMessage,
		    BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified changing force wifi disable to value other than default value.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 13 : Perform Factory reset on the device using SNMP/WEBPA/TR-69 request.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Performing Factory reset on the device using "
		    + factoryResetType + " request.");
	    switch (factoryResetType) {
	    case TestCategory.WEBPA:
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Execute below command -  "
			+ "curl -i -H \"Authorization:Bearer <SAT_TOKEN>\" -X PATCH -H \"Content-Type:application/json\" -H \"Accept:application/json\" -w %{time_total} -k \"https://<url:port>/api/v2/device/mac:<ECM_MAC>/config\" -d '{\"parameters\":[{\"dataType\":0,\"name\":\"Device.X_CISCO_COM_DeviceControl.FactoryReset\":\"Router,Wifi,VoIP,Dect,MoCA\"}]}'");
		LOGGER.info("STEP " + stepNumber
			+ ": EXPECTED : WebPA Command should return success message and log message \"Received reboot_reason as:factory-reset\" should be present in /rdklogs/logs/WEBPAlog.txt.0 file.");
		LOGGER.info("**********************************************************************************");
		status = BroadBandFactoryResetUtils.performFactoryUsingWebpaAndVerifyInBootTimeLog(device, tapEnv);
		errorMessage = "Failed to factory reset wifi interface using WebPa parameter "
			+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET;
		break;
	    case TestCategory.TR69:
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Execute below TR069 command -  "
			+ "curl --tlsv1.2 --request POST --url https://<url>/telescope/api/<model>/<serialnumber>/ --header ': ' --header 'content-type: application/json' --data '{\"parameters\":[{\"dataType\":0,\"name\":\"Device.X_CISCO_COM_DeviceControl.FactoryReset\",\"value\":\"Router,Wifi,VoIP,Dect,MoCA\"}]}'");
		LOGGER.info("STEP " + stepNumber
			+ ": EXPECTED : SNMP Command should successfully get executed and log message \"Received reboot_reason as:factory-reset\" should be present in /rdklogs/logs/WEBPAlog.txt.0 file.");
		LOGGER.info("**********************************************************************************");
		status = BroadBandFactoryResetUtils.performFactoryResetTR69(tapEnv, device);
		errorMessage = "Failed to factory reset using TR69 with parameter "
			+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET;
		if(status){
		    BroadBandTr69Utils.enableOrVerifyTheTr69Configuration(device, tapEnv);
		}
		break;
	    case TestCategory.SNMP:
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute below command -  "
				+ "snmpset -OQ -v 2c -c <community string> udp6:[ecm_ip]:161 1.3.6.1.4.1.17270.50.2.1.1.1002.0 i 1");
			LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : WebPA Command should return success message and log message \"Received reboot_reason as:factory-reset\" should be present in /rdklogs/logs/WEBPAlog.txt.0 file.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.performFactoryUsingSNMPAndVerifyInBootTimeLog(device, tapEnv);
			if (status) {
			    LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully performed factory reset via "
				    + factoryResetType);
			} else {
			    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			errorMessage = "Failed to factory reset wifi interface using snmp OID "
				+ BroadBandSnmpMib.FACTORY_RESET_DEVICE.getOid();
			break;
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    
	    /**
	     * Step 14 : Verify the Current SSID value for 2.4 Ghz using WebPA request..
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :  Verify the Current SSID value for 2.4Ghz and 5 Ghz using WebPA request.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the Webpa Get command for following param: 'Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID' & 'Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  Current SSID value should be the same as the SSID value from step1.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = WifiSsid.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified current SSID value for 2.4GHz is default after factory reset.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 15 : Verify Parental Control service value with the default value
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify Parental Control service value with the default value.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION :Execute the WebPA get command with the below configuration"
		    + "Parameter and Value : \\n" + "1. Parental Control Managed Sites \\n"
		    + "Name : Device.X_Comcast_com_ParentalControl.ManagedSites.Enable \\n"
		    + "2. Parental Control Managed Service \\n"
		    + "Name :Device.X_Comcast_com_ParentalControl.ManagedServices.Enable \\n"
		    + "3. Parental Control Managed Devices \\n"
		    + "Name : Device.X_Comcast_com_ParentalControl.ManagedDevices.Enable");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Default value and current value of the Parental Control Services should be the same.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = ParentalControlService.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified parental control value is default after factory reset.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 16 : Verify Advanced feature service value with default value.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify Advanced feature service value with default value.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION :Execute the WebPA get command with the below configuration"
		    + "Parameter \\n" + "1. Port Mapping \\n" + "Name : Device.NAT.X_Comcast_com_EnablePortMapping,\\n"
		    + "2. HS  Port Mapping -  \\n" + "Name : Device.NAT.X_Comcast_com_EnableHSPortMapping,\\n"
		    + "3. Port Trigger \\n" + "Name : Device.NAT.X_CISCO_COM_PortTriggers.Enable,\\n"
		    + "4.Remote Management \\n" + "Name : Device.UserInterface.X_CISCO_COM_RemoteAccess.Enable,\\n"
		    + "5. DMZ \\n" + "Name : Device.NAT.X_CISCO_COM_DMZ.Enable,\\n"
		    + "6.Routing > Authentication Type \\n"
		    + "Name : Device.Routing.RIP.InterfaceSetting.1.X_CISCO_COM_AuthenticationType,\\n"
		    + "7.Dynamic DNS \\n" + "Name : Device.X_CISCO_COM_DDNS.Enable,\\n"
		    + "8.Device Discovery > upnp \\n" + "Name : Device.UPnP.Device.Enable,");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  Default value and current value of the  Advanced feature Services should be the same.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = AdvancedFeatureServices.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified Advanced feature service is default after factory reset.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 17 : Verify Local Gateway IPV4, DHCP start and end value with default value.
	     */
	    
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify Local Gateway IPV4, DHCP start and end value with default value.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION :Execute the WebPA set command with the below configuration"
		    + "Parameter and Value : \\n" + "1. LOCAL GATEWAY IP \\n"
		    + "Name : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanIPAddress, \\n"
		    + "2. LOCAL DHCP start address \\n" + "Name :Device.DHCPv4.Server.Pool.1.MinAddress, \\n"
		    + "3. LOCAL DHCP end address \\n" + "Name : Device.DHCPv4.Server.Pool.1.MaxAddress");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Default value and current value of the  Local Gateway IPV4, DHCP start and end value should be the same.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = LocalGatewayIPv4.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified Local Gateway IPV4,DHCP start and DHCP end value is default after factory reset.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 18 : Verify the Current SSID value for 2.4 Ghz using SNMP request.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :  Verify the Current SSID value for 2.4Ghz and 5 Ghz using SNMP request.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION :Execute the SNMP Get command for 2.4 Ghz and 5Ghz using SNMP OID.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  Current SSID value should be the same as the SSID value from step1.");
	    LOGGER.info("**********************************************************************************");
	    if (!DeviceModeHandler.isDSLDevice(device)) {
		factoryResetSettings = WifiSsidSnmp.values();
		errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
		status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
		if (status) {
		    LOGGER.info("STEP " + stepNumber
			    + " : ACTUAL : Successfully verified Local Gateway IPV4,DHCP start and DHCP end value is default after factory reset.");
		} else {
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    /**
	     * Step 19 : Verify 2.4 GHz channel bandwidth value is 20/40 MHz.
	     */
	    stepNumber = "s" + (++stepNum);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify 2.4 GHz channel bandwidth value is 20/40 MHz.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Get channel bandwidth value using webpa for the parameter Device.WiFi.Radio.10000.OperatingChannelBandwidth.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  WebPA response should be successful and value should be 40Mhz.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = WifiSettings2Ghz.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully verified 2.4GHZ channel bandwidth value is default after factory reset");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 21 : Verify that the CodeBigFirst Enable has default value
	     */
	    stepNumber = "s" + ++stepNum;
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify that the CodeBigFirst Enable has default value.");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute following dmcli command: dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable");
	    LOGGER.info("STEP " + stepNum + ": EXPECTED : Successfully verified default value of CodeBig Enable.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = CodeBig.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Successfully verified default value of CodeBig Enable.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 22 : Verify that the TelemetryEndpoint Enable has default value
	     */
	    stepNumber = "s" + ++stepNum;
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNum + ": DESCRIPTION : Verify that the TelemetryEndpoint Enable has default value.");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute following dmcli command: dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
	    LOGGER.info("STEP " + stepNum
		    + ": EXPECTED : Successfully verified default value of TelemetryEndpoint Enable.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = TelemetryEndpoint.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
	    
	    // if telemetry endpoint is enabled via RFC.Then Default value of
	    // TELEMETRY_ENDPOINT_ENABLE can be retrived from /rdklogs/logs/dcmrfc.log
	    if (!status) {
		String defaultValue = TelemetryEndpoint.TELEMETRY_ENDPOINT_ENABLE.getDefaultValue(tapEnv, device);
		String searchValue = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.PATTERN_TO_GREP_TELEMETRY_ENDPOINT_ENABLE,
			BroadBandTestConstants.DCMRFC_LOG_FILE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
		if (CommonMethods.isNotNull(searchValue) && CommonMethods.isNotNull(defaultValue)) {
		    searchValue = CommonMethods.patternFinder(searchValue,
			    BroadBandTestConstants.PATTERN_TO_FIND_TELEMETRY_ENDPOINT_ENABLE);
		}
		status = CommonMethods.isNotNull(searchValue)
			&& CommonMethods.patternMatcher(defaultValue, searchValue);
	    }
	    
	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : Successfully verified default value of TelemetryEndpoint Enable.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 23 : Verify the default value of Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection
	     * is FALSE after Factory Reset. This step is only for specific devices
	     */
	    stepNumber = "s" + ++stepNum;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum
		    + ": DESCRIPTION : Verify the default value of LimitBeaconDetection is false after Factory Reset.");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute the Webpa Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection");
	    LOGGER.info("STEP " + stepNum
		    + ": EXPECTED : Default value of LimitBeaconDetection should be retrieved as false.");
	    LOGGER.info("**********************************************************************************");
	    
		isSupportedDevices = null;
		
		isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

		if (isSupportedDevices) 
		{
		factoryResetSettings = LimitBeaconDetectionEnum.values();
		errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
		status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + " : ACTUAL : Default value  of LimitBeaconDetection is retrieved as FALSE after factory reset.");
		} else {
		    LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    /**
	     * Step 24 : Verify that the rabid framework memory limit has default value
	     */
	    stepNumber = "s" + ++stepNum;
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum
		    + ": DESCRIPTION : Verify that the Rabid framework memory limit has default value.");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute following dmcli command: dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.MemoryLimit");
	    LOGGER.info("STEP " + stepNum
		    + ": EXPECTED : Default value of rabid framework memory limit should be verified.");
	    LOGGER.info("**********************************************************************************");
	    
	    // TR-181 to configure "MemoryLimit" for Rabid is not applicable for Business class devices
	    if (DeviceModeHandler.isDSLDevice(device) || DeviceModeHandler.isBusinessClassDevice(device)) {
		LOGGER.info("This Step is not applicable for DSL and Business class devices");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			"Step is not applicable for DSL and Business class devices", false);
	    } else {
		factoryResetSettings = RabidMemoryLimit.values();
		errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
		status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : Successfully verified default value of Rabid framework memory limit");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    }

	    /**
	     * Step 25 : Verify that Force WiFi Disable has default value
	     */
	    stepNumber = "s" + ++stepNum;
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify that Force WiFi Disable has default value.");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute the Webpa Get command for following param: Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable");
	    LOGGER.info("STEP " + stepNum + ": EXPECTED : Default value of force wifi disable should verified.");
	    LOGGER.info("**********************************************************************************");
	    factoryResetSettings = ForceWiFiDisable.values();
	    errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
	    status = !CommonUtils.isGivenStringAvailableInCommandOutput(errorMessage,
		    BroadBandTestConstants.STRING_FAILED);
	    if (status) {
		LOGGER.info(
			"STEP " + stepNum + ": ACTUAL : Successfully verified default value of force wifi disable.");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	}catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING FACTORY REBOOT THROUGH " + factoryResetType + " : "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	}finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("Checking whether device is Accessible");
	    status = CommonMethods.isSTBAccessible(device);
	    if (!status) {
		status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	    }
	    if (status) {
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		if (status) {
		    BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
		} 
		else {
		    LOGGER.error("Could not start the reactivation sequence"
			    + " since webpa is not UP for the device after factory reboot");
		} 
	    } else {
		LOGGER.error(
			"Failed to reactivate the Broadband Device using WebPA Parameters, as device is not accessible.");
	    }
	    LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");
	}
    }
    
    /**
 * Helper method to get default values of all setting in MAP
 * 
 * @param device
 *            Dut instance
 * @return default value of settings with key and pair
 * @author Praveenkumar Paneerselvam
 * @Refactor Athira
 */
private Map<FactoryResetSettings, String> getDefaultValuesInMap(Dut device, int preConditionNumber) {
// Variable declaration starts
// Map containing default values for factory reset setting option.
Map<FactoryResetSettings, String> defaultValues = new HashMap<FactoryResetSettings, String>();
String errorMessage = null;
boolean status = false;
String response = null;
// Variable declaration ends
//PC2
/**
 * Pre-Condition : Getting default value for 2.4GHz & 5GHz SSID via WebPa.
 */
preConditionNumber++;
LOGGER.info("#######################################################################################");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ ": DESCRIPTION : Verify getting default SSID values for 2.4GHz and 5GHz using WebPA request.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION  :Execute the WebPA Get command for following params: 'Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID' & 'Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID'");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : EXPECTED  : WebPA Get request should return default SSID value with success message.");
LOGGER.info("#######################################################################################");
// Pre- Condition to Get default values for WiFi SSID via WebPa
FactoryResetSettings[] factoryResetSettings = WifiSsid.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");

//PC3
/**
 * Pre-Condition : Getting default value for 2.4GHz & 5GHz SSID via SNMP.
 */
if (!DeviceModeHandler.isDSLDevice(device)) {
    preConditionNumber++;
    LOGGER.info("#######################################################################################");
    LOGGER.info("PRE-CONDITION " + preConditionNumber
	    + ": DESCRIPTION : Verify getting default SSID values for both 2.4GHz & 5GHz via SNMP.");
    LOGGER.info("PRE-CONDITION " + preConditionNumber
	    + ": ACTION :Excecute SNMP Get command for following OID for both SSIDs: 1.3.6.1.4.1.17270.50.2.2.2.1.1.16.");
    LOGGER.info("PRE-CONDITION " + preConditionNumber
	    + " : EXPECTED : SNMP Get request should be successful and return default value for both SSIDs.");
    LOGGER.info("#######################################################################################");
    // Pre- Condition to get default values for Wifi SSID via SNMP
    factoryResetSettings = WifiSsidSnmp.values();
    BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
    LOGGER.info("#######################################################################################");
}
//PC4
/**
 * Pre-Condition : Getting default values for Parental Control Services
 */
preConditionNumber++;
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify getting default values for Parental Control Services.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION : SSH the device & Execute following commands: \\n" + "1. Managed Sites  \\n"
	+ "grep -i managedsites_enabled= /etc/utopia/system_defaults  \\n" + "2. Managed Services - \\n"
	+ "grep -i managedservices_enabled= /etc/utopia/system_defaults \\n" + "3. Managed Devices \\n"
	+ "grep -i manageddevices_enabled= /etc/utopia/system_defaults \\n");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : EXPECTED : Default value for all Parental Control parameter should be obtained successfully.");
LOGGER.info("#######################################################################################");
// Pre-Condition to Get default values for Parental Control Services
factoryResetSettings = ParentalControlService.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");
//PC5
/**
 * Pre-Condition : Getting default values for Advanced Features Services
 */
preConditionNumber++;
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify getting default values for Advanced feature Services.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION : SSH the device & Execute following commands: \\n" + "1. Port Mapping  \\n"
	+ "grep -i port_forward_enabled=  /etc/utopia/system_defaults  \\n" + "2. HS  Port Mapping - \\n"
	+ "grep -i port_hs_forward_enabled=  /etc/utopia/system_defaults \\n" + "3. Port Trigger \\n"
	+ "grep -i port_trigger_enabled=  /etc/utopia/system_defaults \\n" + "5. DMZ\\n"
	+ "grep -i dmz_enabled= /etc/utopia/system_defaults \\n" + "7.Dynamic DNS\\n"
	+ "grep -i ddns_enable= /etc/utopia/system_defaults\\n" + "8.Device Discovery > upnp\\n"
	+ "grep -i upnp_igd_enabled= /etc/utopia/system_defaults");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : EXPECTED : Default value for all Advanced feature services parameter should be obtained successfully.");
LOGGER.info("#######################################################################################");
// Pre-Condition to Get default values for Advanced Feature Services
factoryResetSettings = AdvancedFeatureServices.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");
//PC6
/**
 * Pre-Condition : Getting Local Gateway IPv4, DHCP LOCAL IPv4 Start, DHCP LOCAL IPv4 End
 */
preConditionNumber++;
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify getting default values for LOCAL Gateway IPv4, DHCP LOCAL IPv4 Start, DHCP LOCAL IPv4 End.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION : Execute below commands to get default value for Advanced feature services \\n"
	+ "1. Local Gateway IP  from " + BroadBandWebPaConstants.WEBPA_PARAM_DEFAULT_LANIP_FOR_SYNDICATION
	+ " /nvram/partners_defaults.json  \\n" + "2. Local IP DHCP start from "
	+ BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS
	+ "  /nvram/partners_defaults.json \\n" + "3. Local IP DHCP end from"
	+ BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS
	+ "  /nvram/partners_defaults.json \\n");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : EXPECTED : Default value for all parameter should be obtained successfully. ");
LOGGER.info("#######################################################################################");
// Pre- Condition to get default values for LOCAL Gateway IPv4, DHCP LOCAL
// IPv4 Start, DHCP LOCAL IPv4 End
response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
factoryResetSettings = LocalGatewayIPv4.values();

status = compareDhcpDefaultValues(tapEnv, device);
if (status) {
    BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
} else {
    errorMessage = " LocalGatewayIP,DHCP start,DHCP end value in default file and expected default value are not same ";
    throw new TestException(
	    BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION  : FAILED : " + errorMessage);
}
LOGGER.info("#######################################################################################");

//PC6
/**
 * Pre-Condition : Getting default value for 2.4GHz Channel bandwidth.
 */

preConditionNumber++;
LOGGER.info("#######################################################################################");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify getting default value for 2.4GHz Channel bandwidth.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION :Execute the WebPA Get command for following param: Device.WiFi.Radio.10000.OperatingChannelBandwidth");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ ": EXPECTED : Default value for 2.4GHz channel bandwidth should be set to 40MHz.");
LOGGER.info("#######################################################################################");
// Default value is hardcoded as 40MHz
factoryResetSettings = WifiSettings2Ghz.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");

/**
 * Pre-Condition : Getting default value for CodeBig Enable
 */
preConditionNumber++;
LOGGER.info("#######################################################################################");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify CodeBig Enable is disabled by default.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION : Execute the WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable");
LOGGER.info("PRE-CONDITION " + preConditionNumber + " : EXPECTED : Code Big should be disabled by default.");
LOGGER.info("#######################################################################################");
// Default value is false
factoryResetSettings = CodeBig.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");


/**
 * Pre-Condition : Getting default value for Telemetry Endpoint
 */
preConditionNumber++;
LOGGER.info("#######################################################################################");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify Telemetry Endpoint is disabled by default.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION :Execute the WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : EXPECTED : Telemetry Endpoint should be disabled by default.");
LOGGER.info("#######################################################################################");
// Default value is false 
factoryResetSettings = TelemetryEndpoint.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");

/**
 * Getting default value for LimitBeaconDetection
 */

Boolean isLimitBeaconDevice = null;
isLimitBeaconDevice = BroadbandPropertyFileHandler.isDeviceLimitBeacon(device);

if (isLimitBeaconDevice){
    preConditionNumber++;
    LOGGER.info("***************************************************************************************");
    LOGGER.info("PRE-CONDITION " + preConditionNumber
	    + " DESCRIPTION:  Verify getting default value for LimitBeaconDetection.");
    LOGGER.info("PRE-CONDITION " + preConditionNumber
	    + " ACTION : Execute WebPa Get comamnd for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection");
    LOGGER.info("PRE-CONDITION " + preConditionNumber
	    + " EXPTECTED: LimitBeaconDetection should be false by default.");
    LOGGER.info("*********************************************************************************");

    // Defaulf value of LimitBeaconDetectionEnum is false 
    factoryResetSettings = LimitBeaconDetectionEnum.values();
    BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
}
LOGGER.info("#######################################################################################");
/**
 * Pre-Condition : Getting Rabid Framework memory limit
 */
preConditionNumber++;
LOGGER.info("#######################################################################################");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify getting default value for rabid framework memory limit.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION :Execute WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.MemoryLimit");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : EXPECTED : Rabid framework memory limit should be 20MB by default.");
LOGGER.info("#######################################################################################");
// Default value is false 
factoryResetSettings = RabidMemoryLimit.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");

/**
 * Pre-Condition : Getting Force WiFi default value
 */
LOGGER.info("#######################################################################################");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : DESCRIPTION : Verify force wifi disable is disabled by default.");
LOGGER.info("PRE-CONDITION " + preConditionNumber
	+ " : ACTION :Execute WebPA Get command for following param:  Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable");
LOGGER.info(
	"PRE-CONDITION " + preConditionNumber + " : EXPECTED : Force wifi disable should be false by default.");
LOGGER.info("#######################################################################################");
// Default value is false 
factoryResetSettings = ForceWiFiDisable.values();
BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
LOGGER.info("#######################################################################################");

return defaultValues;

}

/**
 * Helper method to compare Lan IP,DHCP start,DHCP end value with default values
 * 
 * @param device
 *            instance of {@link Dut}
 * @param tapEnv
 *            instance of {@link AutomaticsTapApi}
 * @return true if default and obtained value is same false-if they are different
 */
public static boolean compareDhcpDefaultValues(AutomaticsTapApi tapEnv, Dut device) {
boolean status = false;
String response = null;
boolean isLanIp = false;
boolean isDhcpStart = false;
boolean isDhcpEnd = false;
try {
    response = (DeviceModeHandler.isBusinessClassDevice(device))
		    ? BroadBandTestConstants.COMMERCIAL
		    : tapEnv.executeWebPaCommand(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);

    if (CommonMethods.isNotNull(response)) {
	isLanIp = CommonMethods
		.isNotNull(BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_GATEWAY_IP.getDefaultValue(tapEnv,
				device))
		&& CommonUtils.patternSearchFromTargetString(
			BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_GATEWAY_IP.getDefaultValue(tapEnv,
					device),
			BroadBandCommonUtils.getSynLocalIPforPartner(response));

	isDhcpStart = CommonMethods
		.isNotNull(BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_DHCP_START.getDefaultValue(tapEnv,
			device))
		&& CommonUtils.patternSearchFromTargetString(
			BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_DHCP_START.getDefaultValue(tapEnv,
				device),
			BroadBandCommonUtils.getSynStartIPforPartner(response));

	isDhcpEnd = CommonMethods
		.isNotNull(BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_DHCP_END.getDefaultValue(tapEnv,
			device))
		&& CommonUtils.patternSearchFromTargetString(
			LocalGatewayIPv4.LAN_DHCP_END.getDefaultValue(tapEnv, device),
			BroadBandCommonUtils.getSynEndIPforPartner(response));
	status = isLanIp && isDhcpStart && isDhcpEnd;
    }
} catch (Exception exception) {
    LOGGER.error("Exception occurred while validating LocalGatewayIp,DHCP start value,DHCP end value"
	    + exception.getMessage());
}
return status;
}
/**
* Method to validate set value with default value on device
* 
* @param device
*            Dut Instance
* @param factoryResetSettings
*            factory reset setting option for which validation comparison
*            has to be done
* @param defaultValues
*            default value for the particular setting.
* @param skipList
*            has settings value for which new value is not set.
* @return error or success message.
* @author Praveenkumar Paneerselvam
* @Refactor Athira
*/
private String validateSettingsOnDevice(Dut device, FactoryResetSettings[] factoryResetSettings,
	Map<FactoryResetSettings, String> defaultValues, List<FactoryResetSettings> skipList) {
StringBuilder message = new StringBuilder();
LOGGER.debug("Inside Method : validateSettingsOnDevice()");
String defaultValue = null;
for (FactoryResetSettings factoryResetSetting : factoryResetSettings) {

	if (!skipList.contains(factoryResetSetting)) {
		defaultValue = defaultValues.get(factoryResetSetting);
		LOGGER.info("Default value for the parameter " + factoryResetSetting + " is " + defaultValue);
		if (CommonMethods.isNotNull(defaultValue)) {
			String response = null;
			if (factoryResetSetting.equals(WifiSsidSnmp.WIFI_SSID_2_4)
					|| factoryResetSetting.equals(WifiSsidSnmp.WIFI_SSID_5)) {
				LOGGER.info("Inside wifi ssid value for snmp");
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_CURRENT_SSID.getOid(),
						factoryResetSetting.getSystemDefaultVariable());
				LOGGER.info("SNMP Response for current value of parameter " + factoryResetSetting + ": "
						+ response);
			} else {
				response = tapEnv.executeWebPaCommand(device, factoryResetSetting.getWebpaParameter());
			}

			if (CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response,
							BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
					&& !CommonMethods.patternMatcher(response,
							BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
					&& !CommonMethods.patternMatcher(response,
							BroadBandSnmpConstants.SNMP_ERROR_NON_WRITABLE)) {
				if (response.equals(defaultValue)) {
					message.append(
							"SUCCESS : Current value and Default value after factory reset for the parameter "
									+ factoryResetSetting + " is same."
									+ BroadBandTestConstants.DELIMITER_NEW_LINE);
				} else {
					message.append(
							"FAILED : Current value and Default value after factory reset for the parameter "
									+ factoryResetSetting + " is not same."
									+ BroadBandTestConstants.DELIMITER_NEW_LINE);
				}
			} else {
				message.append(
						"FAILED: Current Value is not obtained for parameter " + factoryResetSetting.toString()
								+ " Skipping step to validate value after factory reset "
								+ BroadBandTestConstants.DELIMITER_NEW_LINE);
			}
		} else {
			message.append("FAILED: Default Value is not obtained for parameter "
					+ factoryResetSetting.toString() + " Skipping step to validate value after factory reset n"
					+ BroadBandTestConstants.DELIMITER_NEW_LINE);
		}
	} else {
		message.append("FAILED: Failed to set value before factory reset for the parameter "
				+ factoryResetSetting.toString() + " Skipping step to validate value after factory reset "
				+ BroadBandTestConstants.DELIMITER_NEW_LINE);
	}

}
LOGGER.info(message.toString());
LOGGER.debug("Exit from Method : validateSettingsOnDevice()");
return message.toString();
}
}
